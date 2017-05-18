
//-----------------------------------------------------------------------------
// Optional compiler directives                                                
//-----------------------------------------------------------------------------

{$ASSERTIONS OFF}
{$DEBUGINFO ON}
{$LOCALSYMBOLS ON}
{$STACKFRAMES OFF}
{$DEFINITIONINFO OFF}

{$IFDEF VER130}
  {$DEFINE D5}
  {$DEFINE D5_UP}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE D5_UP}
  {$DEFINE D6}
  {$DEFINE D6_UP}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE D5_UP}
  {$DEFINE D6_UP}
  {$DEFINE D7}
  {$DEFINE D7_UP}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE D5_UP}
  {$DEFINE D6_UP}
  {$DEFINE D7_UP}
  {$DEFINE D2007}    
{$ENDIF}

{$IFDEF D6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

// $C - Assertions
// $D - Debug info
// $L - Local information
// $W - Stack frames
// $Y - Symbol declaration

//-----------------------------------------------------------------------------
// Compiler directives that do not need to be changed                          
//-----------------------------------------------------------------------------

{$A+,B-,E-,F-,G+,H+,I+,J+,K-,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,X+,Z1}

// $A - Align
// $B - Full boolean evaluation
// $E - Executable extension
// $F - ### Unknown
// $G - Imported data
// $H - Long strings
// $I - I/O checks
// $J - Writable typed constants
// $K - ### Unknown
// $M - RTTI
// $N - ### Unknown
// $O - Optimization
// $P - Open strings parameters
// $Q - Overflow checking
// $R - Range checking
// $S - ### Unknown
// $T - Type checked pointers
// $U - Pentium safe FDIV
// $V - Var string checking
// $X - Extended syntax
// $Z - Minimum enumeration size


{$MINSTACKSIZE $00004000}

{$MAXSTACKSIZE $00100000}

{$IMAGEBASE $00400000}

{.$DEFINE CHECK_387_CONTROL}

{$DEFINE FIX_387_CONTROL}

{$DEFINE CLEAR_387_STATUS}

{.$DEFINE USE_DELPHI_MATH_POWER}

{.$DEFINE USE_DELPHI_INT_POWER}

{$DEFINE USE_DB}

{.$DEFINE USE_FAST_MATH}

{$DEFINE USE_SAFE_ROUND}






