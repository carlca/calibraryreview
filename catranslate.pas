unit caTranslate;

{$INCLUDE ca.inc}

interface

uses

  // standard delphi units...
  Sysutils,
  Windows,
  Classes,
  Forms,
  TypInfo,
  IniFiles,
  FileCtrl,

  // ca units...
  caConsts,
  caUtils,
  caForms,
  caLog,
  caIni;

type

  TcaLanguage = (laEnglish, laFrench, laGerman, laAmerican);

  //---------------------------------------------------------------------------
  // TcaApplicationProxy
  //---------------------------------------------------------------------------

  TcaApplicationProxy = class(TObject)
  private
    // private fields...
    FExcludedProperties: TStringList;
    FLanguage: TcaLanguage;
    FLanguageRoot: string;
    FDFMFile: TcaDFMFile;
    FDictionary: THashedStringList;
    // private methods...
    function GetLanguageFile(ALanguage: TcaLanguage): string;
    function GetLanguageFolder(ALanguage: TcaLanguage): string;
    procedure InitializeLanguages;
    procedure ReadSelectedLanguage;
    procedure SaveLanguageFile;
    function GetMainFormOnTaskbar: Boolean;
    procedure SetMainFormOnTaskbar(const Value: Boolean);
  protected
    // protected methods...
    procedure LoadLanguageFile;
    procedure SaveSelectedLanguage;
  public
    // lifetime...
    constructor Create;
    destructor Destroy; override;
    // public methods
    procedure AddExcludedProperty(const AProperty: string);
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure Initialize;
    procedure Run;
    procedure TranslateString(const APropPath: String; var APropValue: String);
    procedure TranslateStringEvent(Sender: TObject; const APropPath: String; var APropValue: String);
    // properties
    property DFMFile: TcaDFMFile read FDFMFile;
    property Language: TcaLanguage read FLanguage write FLanguage;
    property MainFormOnTaskbar: Boolean read GetMainFormOnTaskbar write SetMainFormOnTaskbar;
  end;

  //---------------------------------------------------------------------------
  // TcaTranslator                                                            
  //---------------------------------------------------------------------------

  TcaTranslator = class(TObject)
  public
    // public methods...
    procedure AddExcludedProperty(const AProperty: string);
    function CreateForm(AFormClass: TFormClass; AOwner: TComponent;
      ATranslateEvent: TcaDFMTranslateStringEvent = nil): Forms.TForm;
    procedure LoadLanguageFile;
    procedure TranslateString(const APropPath: String; var APropValue: String);
  end;

  //---------------------------------------------------------------------------
  // TcaApplicationAccess
  //---------------------------------------------------------------------------

  TcaApplicationAccess = class(TComponent)
  private
    // private fields - copied from Forms.TApplication...
    FHandle: HWnd;
    FBiDiMode: TBiDiMode;
    FBiDiKeyboard: string;
    FNonBiDiKeyboard: string;
    FObjectInstance: Pointer;
    FMainForm: TForm;
  protected
    // protected properties - Used to supress compiler hints...
    property BiDiKeyboard: string read FBiDiKeyboard;
    property BiDiMode: TBiDiMode read FBiDiMode;
    property Handle: HWnd read FHandle;
    property NonBiDiKeyboard: string read FNonBiDiKeyboard;
    property ObjectInstance: Pointer read FObjectInstance;
  public
    // public properties...
    property MainForm: TForm read FMainForm write FMainForm;
  end;

  //---------------------------------------------------------------------------
  // TForm                                                                    
  //---------------------------------------------------------------------------

  TForm = class(Forms.TForm)
  public
    // lifetime...
    constructor Create(AOwner: TComponent); override;
  end;

var
  Application: TcaApplicationProxy;
  Translator: TcaTranslator;

const
  cSelectedLangauge     = 'SelectedLangauge';

implementation

//---------------------------------------------------------------------------
// TcaApplicationProxy
//---------------------------------------------------------------------------

// lifetime...

constructor TcaApplicationProxy.Create;
begin
  inherited;
  InitializeLanguages;
  ReadSelectedLanguage;
  FDictionary := THashedStringList.Create;
  FDFMFile := TcaDFMFile.Create(nil);
  FDFMFile.OnTranslateString := TranslateStringEvent;
  FExcludedProperties := TStringList.Create;
end;

destructor TcaApplicationProxy.Destroy;
begin
  SaveLanguageFile;
  FDictionary.Free;
  FDFMFile.Free;
  FExcludedProperties.Free;
  SaveSelectedLanguage;
  inherited;
end;

// public methods...

procedure TcaApplicationProxy.AddExcludedProperty(const AProperty: string);
begin
  FExcludedProperties.Add(AProperty);
end;

procedure TcaApplicationProxy.CreateForm(InstanceClass: TComponentClass; var Reference);
var
  Instance: TComponent;
begin
  FDFMFile.FormClass := TFormClass(InstanceClass);
  try
    FDFMFile.Translate;
    Instance := FDFMFile.CreateForm(Forms.Application);
  except
    TComponent(Reference) := nil;
    raise;
  end;
  if (Forms.Application.MainForm = nil) and (Instance is Forms.TForm) then
    begin
      TForm(Instance).HandleNeeded;
      TcaApplicationAccess(Forms.Application).MainForm := TForm(Instance);
    end;
end;

procedure TcaApplicationProxy.Initialize;
begin
  Forms.Application.Initialize;
end;

procedure TcaApplicationProxy.Run;
begin
  Forms.Application.Run;
end;

// protected methods...

procedure TcaApplicationProxy.LoadLanguageFile;
var
  LanguageFile: string;
begin
  LanguageFile := GetLanguageFile(FLanguage);
  if FileExists(LanguageFile) then
    FDictionary.LoadFromFile(LanguageFile);
end;

procedure TcaApplicationProxy.SaveSelectedLanguage;
var
  Ini: IcaIni;
begin
  Ini := TcaIni.Create;
  Ini.Integers[cSelectedLangauge] := Ord(FLanguage);
end;

// private methods...

function TcaApplicationProxy.GetLanguageFile(ALanguage: TcaLanguage): string;
begin
  Result := GetLanguageFolder(ALanguage) + cDictionaryFile;
end;

function TcaApplicationProxy.GetLanguageFolder(ALanguage: TcaLanguage): string;
var
  LanguageName: string;
begin
  LanguageName := GetEnumName(TypeInfo(TcaLanguage), Ord(ALanguage));
  Utils.StripLeadingLowerCase(LanguageName);
  Result := FLanguageRoot + LanguageName;
  Utils.EnsureBackslash(Result);
end;

procedure TcaApplicationProxy.InitializeLanguages;
var
  ALanguage: TcaLanguage;
  LanguageFolder: string;
begin
  FLanguageRoot := Utils.AppPath + cLanguages;
  if not DirectoryExists(FLanguageRoot) then
    CreateDir(FLanguageRoot);
  Utils.EnsureBackslash(FLanguageRoot);
  for ALanguage := Low(TcaLanguage) to High(TcaLanguage) do
    begin
      LanguageFolder := GetLanguageFolder(ALanguage);
      if not DirectoryExists(LanguageFolder) then
        CreateDir(LanguageFolder);
    end;
end;

procedure TcaApplicationProxy.ReadSelectedLanguage;
var
  Ini: IcaIni;
begin
  Ini := TcaIni.Create;
  FLanguage := TcaLanguage(Ini.Integers[cSelectedLangauge]);
end;

procedure TcaApplicationProxy.SaveLanguageFile;
var
  LanguageFile: string;
begin
  LanguageFile := GetLanguageFile(FLanguage);
  if not FileExists(LanguageFile) then
    FDictionary.SaveToFile(LanguageFile);
end;

procedure TcaApplicationProxy.TranslateString(const APropPath: string; var APropValue: string);
var
  ExcludeIndex: Integer;
  ExcludeProp: string;
  Name: string;
  NameIndex: Integer;
  ShouldExclude: Boolean;
  Value: string;
begin
  ShouldExclude := False;
  for ExcludeIndex := 0 to Pred(FExcludedProperties.Count) do
    begin
      ExcludeProp := FExcludedProperties[ExcludeIndex];
      if Pos(ExcludeProp, APropPath) > 0 then
        begin
          ShouldExclude := True;
          Break;
        end;
    end;
  if not ShouldExclude then
    begin
      Name := APropValue;
      NameIndex := FDictionary.IndexOfName(Name);
      if NameIndex >= 0 then
        begin
          Value := FDictionary.ValueFromIndex[NameIndex];
          if Value <> cUndefined then
            APropValue := Value;
        end
      else
        begin
          Name := APropPath + ' - ' + APropValue;
          NameIndex := FDictionary.IndexOfName(Name);
          if NameIndex >= 0 then
            begin
              Value := FDictionary.ValueFromIndex[NameIndex];
              if Value <> cUndefined then
                APropValue := Value;
            end
          else
            FDictionary.Values[Name] := cUndefined;
        end;
    end;
end;

procedure TcaApplicationProxy.TranslateStringEvent(Sender: TObject; const APropPath: string; var APropValue: string);
begin
  TranslateString(APropPath, APropValue);
end;

// property methods... 

function TcaApplicationProxy.GetMainFormOnTaskbar: Boolean;
begin
  Result := Forms.Application.MainFormOnTaskBar;
end;

procedure TcaApplicationProxy.SetMainFormOnTaskbar(const Value: Boolean);
begin
  Forms.Application.MainFormOnTaskBar := Value;
end;

//---------------------------------------------------------------------------
// TcaTranslator                                                            
//---------------------------------------------------------------------------

// public methods

procedure TcaTranslator.AddExcludedProperty(const AProperty: string);
begin
  Application.AddExcludedProperty(AProperty);
end;

function TcaTranslator.CreateForm(AFormClass: TFormClass; AOwner: TComponent;
  ATranslateEvent: TcaDFMTranslateStringEvent = nil): Forms.TForm;
var
  DFMFile: IcaDFMFile;
begin
  DFMFile := TcaDFMFile.Create(nil);
  DFMFile.FormClass := AFormClass;
  if Assigned(ATranslateEvent) then
    DFMFile.OnTranslateString := ATranslateEvent
  else
    DFMFile.OnTranslateString := Application.TranslateStringEvent;
  LoadLanguageFile;
  DFMFile.Translate;
  Result := DFMFile.CreateForm(AOwner);
end;

procedure TcaTranslator.LoadLanguageFile;
begin
  Application.LoadLanguageFile;
end;

procedure TcaTranslator.TranslateString(const APropPath: string; var APropValue: string);
begin
  Application.TranslateString(APropPath, APropValue);
end;

//---------------------------------------------------------------------------
// TForm
//---------------------------------------------------------------------------

// lifetime...

constructor TForm.Create(AOwner: TComponent);
begin
  if AOwner is TcaDFMTempOwner then
    inherited Create(AOwner)
  else
    begin
      inherited CreateNew(AOwner);
      Application.DFMFile.FormClass := TFormClass(ClassType);
      Application.DFMFile.Translate;
      Application.DFMFile.Binary.ReadComponent(Self);
    end;
end;

//---------------------------------------------------------------------------
// Initialization/Finalization
//---------------------------------------------------------------------------

initialization
  Application := TcaApplicationProxy.Create;
  Translator := TcaTranslator.Create;

finalization
  Application.Free;
  Translator.Free;

end.

