unit caTypes;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Controls;

type

  //---------------------------------------------------------------------------
  // Array types                                                               
  //---------------------------------------------------------------------------

  TcaWordBoolArray = array[Boolean] of Word;

  TcaIntBoolArray = array[Boolean] of Integer;

  TcaDoubleBoolArray = array[Boolean] of Double;

  TcaMouseMessageArray = array[Low(TMouseButton)..High(TMouseButton)] of Word;

  //---------------------------------------------------------------------------
  // Dynamic array definitions                                                 
  //---------------------------------------------------------------------------

  TcaPointerArray = array of Pointer;

  TcaPointerArray2 = array of array of Pointer;

  TcaObjectArray = array of TObject;

  TcaObjectArray2 = array of array of TObject;

  TcaIntegerArray = array of Integer;

  TcaIntegerArray2 = array of array of Integer;

  TcaDoubleArray = array of Double;

  TcaDoubleArray2 = array of array of Double;

  TcaExtendedArray = array of Extended;

  TcaExtendedArray2 = array of array of Extended;

  TcaStringArray = array of String;

  TcaStringArray2 = array of array of String;

  //---------------------------------------------------------------------------
  // ShortString types                                                         
  //---------------------------------------------------------------------------

  String10 = String[10];
  String20 = String[20];
  String30 = String[30];
  String40 = String[40];
  String50 = String[50];
  String60 = String[60];
  String70 = String[70];
  String80 = String[80];
  String90 = String[90];
  String100 = String[100];
  String110 = String[110];
  String120 = String[120];
  String130 = String[130];
  String140 = String[140];
  String150 = String[150];
  String160 = String[160];
  String170 = String[170];
  String180 = String[180];
  String190 = String[190];
  String200 = String[200];
  String210 = String[210];
  String220 = String[220];
  String230 = String[230];
  String240 = String[240];
  String250 = String[250];

  //---------------------------------------------------------------------------
  // GUID support                                                              
  //---------------------------------------------------------------------------

  TcaUniqueID = string[32];

  //---------------------------------------------------------------------------
  // Time types                                                                
  //---------------------------------------------------------------------------

  TcaTimePoint = packed record
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Min: Word;
    Sec: Word;
    MSec: Word;
  end;

  //---------------------------------------------------------------------------
  // Enumerated types and sets                                                 
  //---------------------------------------------------------------------------

  TcaArrayStringsAlignment = (saLeft, saRight, saPreZero);

  TcaButtonLayout = (laGlyphLeftCentered, laGlyphLeft, laGlyphRight, laGlyphTop, laGlyphBottom, laTextLeft, laTextRight);

  TcaButtonState = (bsUp, bsDisabled, bsDown, bsExclusive);

  TcaButtonStyle = (bsDefault, bsThin, bsFlat, bsNoEdge);

  TcaCompareResult = (crFirstGreater, crSecondGreater, crEqual, crUndefined);

  TcaFrameStyle = (fsLowered, fsRaised, fsLoweredPanel, fsRaisedPanel, fsLine);

  TcaNumGlyphs = 1..2;

  TcaSide = (sdLeft, sdTop, sdRight, sdBottom);

  TcaSides = set of TcaSide;

  TcaSortDirection = (sdAscending, sdDescending, sdNone, sdUndefined);

  TcaTextStyle = (tsNormal, tsRaised, tsLowered);

  TcaMenuState = (meBig, meSmall);

  TcaSizeBarBtnPosition = (spTop, spCenter, spBottom);

  TcaSizeBarKind = (sbHorizontal, sbVertical);

  TcaSizeBarState = (bsBig, bsSmall);

  TcaSizeBarArrows = (baLeftRight, baRightLeft, baUpDown, baDownUp);

  TcaArrowType = (atLeft, atRight, atUp, atDown);

  TcaLetterCase = (caLower, caUpper, caAny);

  TcaXMLTagType = (ttStart, ttEnd, ttEmpty, ttText);

  TcaMsgDialogResponse = (mgNone, mgAbort, mgYes, mgOk, mgRetry, mgNo, mgCancel, mgIgnore, mgAll);

  TcaCellType = (ctNil, ctObject, ctInteger, ctInt64, ctSingle, ctDouble,
                  ctExtended, ctString, ctMemo, ctBoolean, ctDateTime, ctFormula);

  TcaCellTypes = set of TcaCellType;

  TcaChartAxisType = (caLeft, caRight, caTop, caBottom);

  TcaOperatorPrecedence = (opHigher, opLower, opSameLeftAssoc, opSameRightAssoc);

  TcaTrigUnits = (tuDegrees, tuRadians);

  TcaColorColumn = (ccColor, ccName, ccHue, ccSaturation, ccValue, ccIntensity, ccLightness, ccYic);

  TcaNodeCheckState = (csFullyUnChecked, csSemiChecked, csFullyChecked);

  TcaNodeMatchAction = (maCheck, maClear);

  TcaNodeMatchMode = (mmMatch, mmLevel, mmAll);

  TcaNumberType = (ntInteger, ntFloat);

  TcaCaptionMode = (cmText, cmNumber);

  TcaByteSet = set of Byte;

  TcaMathOperation = (moAdd, moDivide, moMultiply, moSubtract);

  TcaMessageDialogResponse = (drFirst, drSecond, drThird, drUndefined);

  TcaWinVersion = (wvUnknown, wvWin95, wvWin98, wvWinNT, wvWin2K, wvWinXP);

  TcaOperator = (opEqual, opLessThan, opGreaterThan, opNotEqual, opGreaterThanOrEqual, opLessThanOrEqual, opUnspecified);

  TcaShowWindow = (swHide, swMaximize, swMinimize, swRestore, swShow, swShowDefault, swShowMaximized,
                   swShowMinimized, swShowMinNoActive, swShowNA, swShowNoActivate, swShowNormal);

  TcaConjunction = (coAnd, coOr);

implementation

end.
