unit PDL.SQL;

interface

uses
  SysUtils, Classes;

type
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
  end;

  IPDLBooleanExpression = interface(IPDLExpression)
    function BoolAnd(AExpression: IPDLExpression): IPDLBooleanExpression;
    function BoolOr(AExpression: IPDLExpression): IPDLBooleanExpression;
  end;

  IPDLFuncs = interface(IPDLExpression)
  end;

  IPDLMathFuncs = interface(IPDLFuncs)
  end;

  IPDLLength = interface(IPDLMathFuncs)
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

  IPDLAbs = interface(IPDLFuncs)
  end;

  IPDLStringFuncs = interface(IPDLFuncs)
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

  IPDLLanguageBool = interface
    function BoolAnd: IPDLBooleanExpression;
    function BoolOr: IPDLBooleanExpression;
  end;

  IPDLLanguage = interface
    function BoolExpression(AExpression: IPDLExpression): IPDLBooleanExpression;
  end;

  TPDLSQL = class sealed
  private
    FDefaultLanguageName: String;
    FLanguages: TStringList;
  public
    class function Instance: TPDLSQL;
    procedure RegisterLanguage(ALanguage: IPDLLanguage);
    property DefaultLanguage: IPDLLanguage read GetDefaultLanguage; default;
    property Language[AName: String]: IPDLLanguage read GetLanguage;
    property DefaultLanguageName: String read FDefaultLanguageName write SetDefaultLanguageName;
  end;

implementation

end.
