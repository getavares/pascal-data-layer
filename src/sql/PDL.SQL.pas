unit PDL.SQL;

interface

uses
  SysUtils, Classes;

type
  EPDLExeception = class(Exception);

  IPDLExpression = interface
    procedure Validate;
    function ToString: String;
  end;

  TPDLExpressionList = class(TInterfaceList)
  public
    function Add(AExpression: IPDLExpression): TPDLExpressionList; overload;
    function Remove(AExpression: IPDLExpression): TPDLExpressionList; overload;
  end;

  IPDLLogicOperator = interface(IPDLExpression)
  end;

  IPDLAnd = interface(IPDLLogicOperator)
  end;

  IPDLOr = interface(IPDLLogicOperator)
  end;

  IPDLNot = interface(IPDLLogicOperator)
  end;

  IPDLComparerOperator = interface(IPDLExpression)
  end;

  IPDLEqual = interface(IPDLComparerOperator)
  end;

  IPDLDifferent = interface(IPDLComparerOperator)
  end;

  IPDLLike = interface(IPDLComparerOperator)
  end;

  IPDLGreaterThan = interface(IPDLComparerOperator)
  end;

  IPDLGreaterEqualThan = interface(IPDLComparerOperator)
  end;

  IPDLLessThan = interface(IPDLComparerOperator)
  end;

  IPDLLessEqualThan = interface(IPDLComparerOperator)
  end;

  IPDLBetween = interface(IPDLComparerOperator)
    function Value1(AExpression: IPDLExpression): IPDLBetween;
    function Value2(AExpression: IPDLExpression): IPDLBetween;
  end;

  IPDLBooleanExpression = interface(IPDLExpression)
    function PDLAnd(AExpression: IPDLExpression): IPDLBooleanExpression;
    function PDLOr(AExpression: IPDLExpression): IPDLBooleanExpression;
  end;

  IPDLFuncs = interface(IPDLExpression)
  end;

  IPDLMathFuncs = interface(IPDLFuncs)
    constructor Create(AExpression: IPDLExression);
  end;

  IPDLMax = interface(IPDLMathFuncs)
  end;

  IPDLMin = interface(IPDLMathFuncs)
  end;

  IPDLSum = interface(IPDLMathFuncs)
  end;

  IPDLAvg = interface(IPDLMathFuncs)
  end;

  IPDLCount = interface(IPDLMathFuncs)
  end;

  IPDLAbs = interface(IPDLMathFuncs)
  end;

  IPDLStringFuncs = interface(IPDLFuncs)
  end;

  IPDLLength = interface(IPDLStringFuncs)
  end;

  IPDLTrim = interface(IPDLStringFuncs)
  end;

  IPDLUpper = interface(IPDLStringFuncs)
  end;

  IPDLLower = interface(IPDLStringFuncs)
  end;

  IPDLDateTimeFuncs = interface(IPDLFuncs)
  end;

  IPDLCurrentDate = interface(IPDLDateTimeFuncs)
  end;

  IPDLCurrentTime = interface(IPDLDateTimeFuncs)
  end;

  IPDLCurrentDateTime = interface(IPDLDateTimeFuncs)
  end;

  IPDLConversionsFuncs = interface(IPDLFuncs)
  end;

  IPDLCoalesce = interface(IPDLConversionsFuncs)
  end;

  IPDLConditionalFuncs = interface(IPDLFuncs)
  end;

  IPDLCase = interface(IPDLConditionalFuncs)
    function When(AExpression: IPDLExpression): IPDLCase
    function CaseElse(AExpression: IPDLExpression): IPDLCase
  end;

  IPDLSelect = interface(IPDLExpression)
    function GetColumns: TPDLExpressionList;
    function GetFrom: IPDLSelect;
    function SetFrom(ASource: IPDLExpression): IPDLSelect;
    function GetJoins: TPDLExpressionList;
    function GetGroupBy: TPDLExpressionList;
    function GetHaving: TPDLExpressionList;
    function GetOrderBy: TPDLExpressionList;
  end;

  IPDLField = interface(IPDLExpression)
  end;

  TPDLFieldList = class(TInterfaceList)
  public
    function Add(AField: IPDLField): TPDLFieldList; overload;
    function Remove(AField: IPDLField): TPDLFieldList; overload;
  end;

  IPDLValue = interface(IPDLExpression)
  end;

  IPDLNull = interface(IPDLExpression)
  end;

  IPDLTable = interface(IPDLExpression)
  end;

  TPDLFieldValueList = class(TInterfaceList)
    function Add(AField: IPDLField; AValue: IPDLExpression): TPDLFieldValueList; overload;
    function Remove(AField: IPDLField): TPDLFieldValueList; overload;
  end;

  IPDLTableConditionExpression = interface(IPDLBooleanExpression)
    function GetTable: IPDLTable;
    procedure SetTable(ATable: IPDLTable);
  end;

  IPDLUpdate = interface(IPDLTableConditionExpression)
    function GetColumns: TPDLFieldList;
  end;

  IPDLDelete = interface(IPDLTableConditionExpression)
  end;

  IPDLInsert = interface(IPDLExpression)
    function GetFields: TPDLFieldValueList
    function GetTable: IPDLTable;
    procedure SetTable(ATable: IPDLTable);
  end;

  IPDLInsertSelect = interface(IPDLExpression)
    function GetFields: TPDLFieldList;
    function GetTable: IPDLTable;
    procedure SetTable(ATable: IPDLTable);
    procedure SetSelect(ASelect: IPDLSelect);
  end;

  IPDLLanguageCommands = interface
    function Select: IPDLSelect;
    function Update: IPDLUpdate;
    function Delete: IPDLDelete;
    function Insert: IPDLInsert;
  end;

  IPDLLanguageLogicOperators = interface
    function PDLAnd: IPDLAnd;
    function PDLOr: IPDLOr;
    function PDLNot: IPDLNot;
  end;

  IPDLLanguageComparerOperators = interface
    function Equal: IPDLEqual;
    function Different: IPDLDifferent;
    function Like: IPDLLike;
    function GreaterThan: IPDLGreaterThan;
    function GreaterEqualThan: IPDLGreaterEqualThan;
    function LessThan: IPDLLessThan;
    function LessEqualThan: IPDLLessEqualThan;
    function Between: IPDLBetween;
  end;

  IPDLLanguageBool = interface
    function PDLAnd(AExpr1: IPDLExpression; AExpr2: IPDLExpression): IPDLBooleanExpression;
    function PDLOr(AExpr1: IPDLExpression; AExpr2: IPDLExpression): IPDLBooleanExpression;
  end;

  IPDLLanguageMathFuncs = interface
    function Max(AExpression: IPDLExpression): IPDLMax;
    function Min(AExpression: IPDLExpression): IPDLMin;
    function Sum(AExpression: IPDLExpression): IPDLSum;
    function Avg(AExpression: IPDLExpression): IPDLAvg;
    function Count(AExpression: IPDLExpression): IPDLCount;
    function Abs(AExpression: IPDLExpression): IPDLAbs;
  end;

  IPDLLanguageStringFuncs = interface
    function Length: IPDLLength;
    function Trim: IPDLTrim;
    function Upper: IPDLUpper;
    function Lower: IPDLLower;
  end;

  IPDLLanguageDateTimeFuncs = interface
    function CurrentDate: IPDLCurrentDate;
    function CurrentTime: IPDLCurrentTime;
    function CurrentDateTime: IPDLCurrentDateTime;
  end;

  IPDLLanguageConversionsFuncs = interface
    function Coalesce: IPDLCoalesce;
  end;

  IPDLLanguageConditionalFuncs = interface
    function PDLCase: IPDLCase;
  end;

  IPDLLanguageFuncs = interface
    function Math: IPDLLanguageMathFuncs;
    function Strings: IPDLLanguageStringFuncs;
    function DateTime: IPDLLanguageDateTimeFuncs;
    function Conversions: IPDLLanguageConversionsFuncs
    function Conditional: IPDLLanguageConditionalFuncs;
  end;

  IPDLLanguage = interface
    function GetName: String;
    function Logic: IPDLLanguageLogicOperators;
    function Comparer: IPDLLanguageComparerOperators;
    function Bool: IPDLLanguageBool; overload;
    function Bool(AExpression: IPDLExpression): IPDLBooleanExpression; overload;
    function Commands: IPDLLanguageCommands;
    function Funcs: IPDLLanguageFuncs;
  end;

  { TPDLSQL }

  TPDLSQL = class sealed
  private
    FDefaultLanguageName: String;
    FLanguages: TStringList;
    function GetDefaultLanguage: IPDLLanguage;
    function GetLanguage(AName: String): IPDLLanguage;
    procedure SetDefaultLanguageName(AValue: String);
  public
    class function Instance: TPDLSQL;
    procedure RegisterLanguage(ALanguage: IPDLLanguage);
    property DefaultLanguage: IPDLLanguage read GetDefaultLanguage; default;
    property Language[AName: String]: IPDLLanguage read GetLanguage;
    property DefaultLanguageName: String read FDefaultLanguageName write SetDefaultLanguageName;
  end;

implementation

var
  PDLSQL: TPDLSQL;

{ TPDLSQL }

function TPDLSQL.GetDefaultLanguage: IPDLLanguage;
begin
  Result:=GetLanguage(FDefaultLanguageName);
end;

function TPDLSQL.GetLanguage(AName: String): IPDLLanguage;
var
  Index: Integer;
begin
  Index:=FLanguages.IndexOf(AName);
  Result:=(FLanguages.Objects[Index] as IPDLLanguage);
end;

procedure TPDLSQL.SetDefaultLanguageName(AValue: String);
begin
  if (FDefaultLanguageName<>AValue) then
    FDefaultLanguageName:=AValue;
end;

class function TPDLSQL.Instance: TPDLSQL;
begin
  if not Assigned(PDLSQL) then
    PDLSQL:=TPDLSQL.Create;
  Result:=PDLSQL;
end;

procedure TPDLSQL.RegisterLanguage(ALanguage: IPDLLanguage);
begin
  if FLanguages.IndexOf(ALanguage.GetName)=-1 then
    begin
    FLanguages.AddObject(ALanguage.GetName, ALanguage);
    if FDefaultLanguageName='' then
      FDefaultLanguageName:=ALanguage.GetName;
    end;
end;

end.
