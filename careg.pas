unit caReg;

{$INCLUDE ca.inc}

interface

procedure Register;

implementation

{$R CACONTROLS.DCR}
{$R CAMENU.DCR}

uses
  Classes,
  {$IFDEF D7_UP}
  DesignIntf,
  DesignEditors,
  VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  ca8087,
  caActionMgr,
  caActionMgrDsgn,
  caActionMgrForm,
  caButtons,
  caChangeParent,
  caChart,
  caChartLegendListBox,
  caCheckBox,
  caClasses,
  caColorPicker,
  caControls,
  caDiagram,
  caDirListView,
  caEdit,
  caEditListBox,
  caFlyout,
  caFont,
  caFormInitializer,
  caFormHook,
  caForms,
  caFormScaler,
  caKeyMonitor,
  caLabel,
  caListView,
  caLocalizer,
  caLog,
  caMarkup,
  // caMemMapImage,
  // caMemMapImageDsgn,
  caMenu,
  caMenuBar,
  caMenuDsgn,
  caNotifyIcon,
  caPageControl,
  caPopupMenu,
  caProgress,
  caRadioButton,
  caRichEd,
  caSizeBar,
  caSizeMove,  
  caSizeMovePanel,
  caSystemMenu,
  caTextStore,
  caTextStoreDsgn,
  caTranslate,
  caTransparentWnd,
  caTrayIcon,
  caTreeView,
  caXMLData,
  caXPControls;

procedure Register;
begin
  RegisterComponents('ca', [
    Tca8087,
    TcaActionManager,
    TcaChangeParent,
    TcaChart,
    TcaChartLegendListBox,
    TcaCheckbox,
    TcaColorPicker,
    TcaComboBox,
    TcaDFMFile,
    TcaDiagram,
    // TcaDiagramElement,
    TcaDiagramController,
    TcaDiagramBox,
    TcaDiagramLineNode,
    TcaDirListView,
    TcaEdit,
    TcaEditListBox,
    TcaFlyout,
    TcaFontController,
    TcaFormInitializer,
    TcaFormHook,
    TcaFormPanel,
    TcaFormScaler,
    TcaGraphicBox,
    TcaKeyMonitor,
    TcaLabel,
    TcaListView,
    TcaLocalizer,
    TcaLogComponent,
    TcaMarkupViewer,
    // TcaMemMapImage,
    TcaMenu,
    TcaMenuBar,
    TcaMenuPage,
    TcaMenuItemButton,
    TcaNotifyIcon,
    TcaPageControl,
    TcaPanel,
    TcaPopupMenu,
    TcaProgressBar,
    TcaRadioButton,
    TcaRepeatSpeedButton,
    TcaRichEdit,
    TcaSizeBar,
    TcaSizeMoveController,
    TcaSizeMovePanel,
    TcaSpacer,
    TcaSpeedButton,
    TcaSplitter,
    TcaSystemMenu,
    TcaTextStore,
    TcaTransparentWnd,
    TcaTrayIcon,
    TcaTreeView,
    TcaXMLDataset,
    TcaXPButton,
    TcaXPCheckbox,
    TcaXPMutex
  ]);
  RegisterNoIcon([TcaTextItem]);
  RegisterComponentEditor(TcaMenu, TcaMenuCompEd);
  RegisterComponentEditor(TcaMenuPage, TcaMenuCompEd);
  RegisterComponentEditor(TcaActionManager, TcaActionMgrCompEd);
  RegisterComponentEditor(TcaTextStore, TcaTextStoreComponentEditor);
  // RegisterPropertyEditor(TypeInfo(TcaBMPFilename), nil, '', TcaBMPFilenameProperty);
  {$IFDEF D7_UP}
  RegisterPropertyInCategory('Visual', TcaPageControl, 'Mode');
  {$ELSE}
  RegisterPropertyInCategory(TVisualCategory, TcaPageControl, 'Mode');
  {$ENDIF}
end;

end.

