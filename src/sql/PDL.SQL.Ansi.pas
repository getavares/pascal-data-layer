unit PDL.SQL.Ansi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PDL.SQL;

type
  EPDLExceptionAnsi = class(EPDLExeception);

  { TPDLExpressionAnsi }

  TPDLExpressionAnsi = class(TInterfacedObject, IPDLExpression)
  protected
    procedure Validate; virtual;
  public
    function ToString: String; virtual;
  end;

  { TPDLAndAnsi }

  TPDLAndAnsi = class(TPDLExpressionAnsi, IPDLAnd)
  public
    function ToString: String; override;
  end;

  { TPDLOrAnsi }

  TPDLOrAnsi = class(TPDLExpressionAnsi, IPDLOr)
  public
    function ToString: String; override;
  end;

  { TPDLNotAnsi }

  TPDLNotAnsi = class(TPDLExpressionAnsi, IPDLNot)
  public
    function ToString: String; override;
  end;

  TPDLComparerOperator = class(TPDLExpressionAnsi, IPDLComparerOperator)
  public
    function ToString: String; virtual; abstract;
  end;

  { TPDLEqual }

  TPDLEqual = class(TPDLComparerOperator, IPDLEqual)
  public
    function ToString: String; override;
  end;

  { TPDLDifferent }

  TPDLDifferent = class(TPDLComparerOperator, IPDLDifferent)
  public
    function ToString: String; override;
  end;

  { TPDLLike }

  TPDLLike = class(TPDLComparerOperator, IPDLLike)
  public
    function ToString: String; override;
  end;

  { TPDLGreaterThan }

  TPDLGreaterThan = class(TPDLComparerOperator, IPDLGreaterThan)
  public
    function ToString: String; override;
  end;

  { TPDLGreaterEqualThan }

  TPDLGreaterEqualThan = class(TPDLComparerOperator, IPDLGreaterEqualThan)
  public
    function ToString: String; override;
  end;

  { TPDLLessThan }

  TPDLLessThan = class(TPDLComparerOperator, IPDLLessThan)
  public
    function ToString: String; override;
  end;

  { TPDLLessEqualThan }

  TPDLLessEqualThan = class(TPDLComparerOperator, IPDLLessEqualThan)
  public
    function ToString: String; override;
  end;

  { TPDLBetween }

  TPDLBetween = class(TPDLComparerOperator, IPDLBetween)
  private
    FValue1: TPDLExpressionAnsi;
    FValue2: TPDLExpressionAnsi;
  protected
    procedure Validate; override;
  public
    constructor Create; override;
    function Value1(AExpression: IPDLExpression): IPDLBetween;
    function Value2(AExpression: IPDLExpression): IPDLBetween;
    function ToString: String; override;
  end;

  { TPDLLogicOperatorsAnsi }

  TPDLLogicOperatorsAnsi = class(TInterfacedObject, IPDLLanguageLogicOperators)
  public
    function PDLAnd: IPDLAnd;
    function PDLOr: IPDLOr;
    function PDLNot: IPDLNot;
  end;

  { TPDLComparerOperatorsAnsi }

  TPDLComparerOperatorsAnsi = class(TInterfacedObject, IPDLLanguageComparerOperators)
  public
    function Equal: IPDLEqual;
    function Different: IPDLDifferent;
    function Like: IPDLLike;
    function GreaterThan: IPDLGreaterThan;
    function GreaterEqualThan: IPDLGreaterEqualThan;
    function LessThan: IPDLLessThan;
    function LessEqualThan: IPDLLessEqualThan;
    function Between: IPDLBetween;
  end;

  { TPDLBooleanExpressionAnsi }

  TPDLBooleanExpressionAnsi = class(TInterfacedObject, IPDLBooleanExpression)
  private
    FExpressions: TPDLExpressionList;
  public
    constructor Create(AExpression: IPDLExpression);
    destructor Destroy; override;
    function PDLAnd(AExpression: IPDLExpression): IPDLBooleanExpression;
    function PDLOr(AExpression: IPDLExpression): IPDLBooleanExpression;
    function ToString: String; override;
  end;

  { TPDLBoolAnsi }

  TPDLBoolAnsi = class(TInterfacedObject, IPDLLanguageBool)
  public
    function PDLAnd(AExpr1: IPDLExpression; AExpr2: IPDLExpression): IPDLBooleanExpression;
    function PDLOr(AExpr1: IPDLExpression; AExpr2: IPDLExpression): IPDLBooleanExpression;
  end;

  TPDLCommandsAnsi = class(TInterfacedObject, IPDLLanguageCommands)
  public
    function Select: IPDLSelect;
    function Update: IPDLUpdate;
    function Delete: IPDLDelete;
    function Insert: IPDLInsert;
  end;

  { TPDLMathFuncsAnsi }

  TPDLMathFuncsAnsi = class(TInterfacedObject, IPDLMathFuncs)
  private
    FExpression: TPDLExpressionAnsi;
  public
    constructor Create(AExpression: IPDLExpression); virtual;
    destructor Destroy; override;
  end;

  { TPDLMaxAnsi }

  TPDLMaxAnsi = class(TPDLMathFuncsAnsi, IPDLMax)
  public
    function ToString: String; override;
  end;

  { TPDLMinAnsi }

  TPDLMinAnsi = class(TPDLMathFuncsAnsi, IPDLMin)
  public
    function ToString: String; override;
  end;

  { TPDLSumAnsi }

  TPDLSumAnsi = class(TPDLMathFuncsAnsi, IPDLSum)
  public
    function ToString: String; override;
  end;

  { TPDLAvgAnsi }

  TPDLAvgAnsi = class(TPDLMathFuncsAnsi, IPDLAvg)
  public
    function ToString: String; override;
  end;

  { TPDLCountAnsi }

  TPDLCountAnsi = class(TPDLMathFuncsAnsi, IPDLCount)
  public
    function ToString: String; override;
  end;

  { TPDLAbsAnsi }

  TPDLAbsAnsi = class(TPDLMathFuncsAnsi, IPDLAbs)
  public
    function ToString: String; override;
  end;

  TPDLLanguageMathFuncsAnsi = class(TInterfacedObject, IPDLLanguageMathFuncs)
  public
    function Max(AExpression: IPDLExpression): IPDLMax;
    function Min(AExpression: IPDLExpression): IPDLMin;
    function Sum(AExpression: IPDLExpression): IPDLSum;
    function Avg(AExpression: IPDLExpression): IPDLAvg;
    function Count(AExpression: IPDLExpression): IPDLCount;
    function Abs(AExpression: IPDLExpression): IPDLAbs;
  end;

  { TPDLLanguageFuncsAnsi }

  TPDLLanguageFuncsAnsi = class(TInterfacedObject, IPDLLanguageFuncs)
  private
    FMath: TPDLLanguageMathFuncsAnsi;
    FStrings: TPDLLanguageStringsFuncsAnsi;
    FDateTime: TPDLLanguageDateTimeFuncsAnsi;
    FConversions: TPDLLanguageConversionsFuncsAnsi;
    FConditional: TPDLLanguageConditionalFuncsAnsi;
  public
    constructor Create;
    destructor Destroy; override;
    function Math: IPDLLanguageMathFuncs;
    function Strings: IPDLLanguageStringFuncs;
    function DateTime: IPDLLanguageDateTimeFuncs;
    function Conversions: IPDLLanguageConversionsFuncs;
    function Conditional: IPDLLanguageConditionalFuncs;
  end;

  { TPDLSQLAnsi }

  TPDLSQLAnsi = class(TInterfacedObject, IPDLLanguage)
  private
    FLogic: TPDLLogicOperatorsAnsi;
    FComparer: TPDLComparerOperatorsAnsi;
    FBool: TPDLBoolAnsi;
    FCommands: TPDLCommandsAnsi;
    FFuncs: TPDLLanguageFuncsAnsi;
  protected
    function GetName: String;
  public
    constructor Create;
    destructor Destroy; override;
    function Logic: IPDLLanguageLogicOperators;
    function Comparer: IPDLLanguageComparerOperators;
    function Bool: IPDLLanguageBool; overload;
    function Bool(AExpression: IPDLExpression): IPDLBooleanExpression; overload;
    function Commands: IPDLLanguageCommands;
    function Funcs: IPDLLanguageFuncs;
  end;

implementation

{ TPDLMathFuncsAnsi }

constructor TPDLMathFuncsAnsi.Create(AExpression: IPDLExpression);
begin
  FExpression:=AExpression;
end;

destructor TPDLMathFuncsAnsi.Destroy;
begin
  FExpression:=nil;
  inherited Destroy;
end;

{ TPDLAbsAnsi }

function TPDLAbsAnsi.ToString: String;
begin
  Result:=' ABS('+FExpression.ToString+') ';
end;

{ TPDLCountAnsi }

function TPDLCountAnsi.ToString: String;
begin
  Result:=' COUNT('+FExpression.ToString+') ';
end;

{ TPDLAvgAnsi }

function TPDLAvgAnsi.ToString: String;
begin
  Result:=' AVG('+FExpression.ToString+') ';
end;

{ TPDLSumAnsi }

function TPDLSumAnsi.ToString: String;
begin
  Result:=' SUM('+FExpression.ToString+') ';
end;

{ TPDLMinAnsi }

function TPDLMinAnsi.ToString: String;
begin
  Result:=' MIN('+FExpression.ToString+') ';
end;

{ TPDLMaxAnsi }

function TPDLMaxAnsi.ToString: String;
begin
  Result:=' MAX('+FExpression.ToString+') ';
end;

{ TPDLMathFuncs }

function TPDLLanguageMathFuncsAnsi.Max(AExpression: IPDLExpression): IPDLMax;
begin
  Result:=TPDLMaxAnsi.Create(AExpression);
end;

function TPDLLanguageMathFuncsAnsi.Min(AExpression: IPDLExpression): IPDLMin;
begin
  Result:=TPDLMinAnsi.Create(AExpression);
end;

function TPDLLanguageMathFuncsAnsi.Sum(AExpression: IPDLExpression): IPDLSum;
begin
  Result:=TPDLSumAnsi.Create(AExpression);
end;

function TPDLLanguageMathFuncsAnsi.Avg(AExpression: IPDLExpression): IPDLAvg;
begin
  Result:=TPDLAvgAnsi.Create(AExpression);
end;

function TPDLLanguageMathFuncsAnsi.Count(AExpression: IPDLExpression): IPDLCount;
begin
  Result:=TPDLCountAnsi.Create(AExpression);
end;

function TPDLLanguageMathFuncsAnsi.Abs(AExpression: IPDLExpression): IPDLAbs;
begin
  Result:=TPDLAbsAnsi.Create(AExpression);
end;

{ TPDLLanguageFuncsAnsi }

function TPDLLanguageFuncsAnsi.Math: IPDLLanguageMathFuncs;
begin

end;

function TPDLLanguageFuncsAnsi.Strings: IPDLLanguageStringFuncs;
begin

end;

function TPDLLanguageFuncsAnsi.DateTime: IPDLLanguageDateTimeFuncs;
begin

end;

function TPDLLanguageFuncsAnsi.Conversions: IPDLLanguageConversionsFuncs;
begin

end;

function TPDLLanguageFuncsAnsi.Conditional: IPDLLanguageConditionalFuncs;
begin

end;

{ TPDLComparerOperatorsAnsi }

function TPDLComparerOperatorsAnsi.Equal: IPDLEqual;
begin
  Result:=TPDLEqual.Create;
end;

function TPDLComparerOperatorsAnsi.Different: IPDLDifferent;
begin
  Result:=TPDLDifferent.Create;
end;

function TPDLComparerOperatorsAnsi.Like: IPDLLike;
begin
  Result:=TPDLLike.Create;
end;

function TPDLComparerOperatorsAnsi.GreaterThan: IPDLGreaterThan;
begin
  Result:=TPDLGreaterThan.Create;
end;

function TPDLComparerOperatorsAnsi.GreaterEqualThan: IPDLGreaterEqualThan;
begin
  Result:=TPDLGreaterEqualThan.Create;
end;

function TPDLComparerOperatorsAnsi.LessThan: IPDLLessThan;
begin
  Result:=TPDLLessThan.Create;
end;

function TPDLComparerOperatorsAnsi.LessEqualThan: IPDLLessEqualThan;
begin
  Result:=TPDLLessEqualThan.Create;
end;

function TPDLComparerOperatorsAnsi.Between: IPDLBetween;
begin
  Result:=TPDLBetween.Create;
end;

{ TPDLExpressionAnsi }

procedure TPDLExpressionAnsi.Validate;
begin

end;

function TPDLExpressionAnsi.ToString: String;
begin
Validate;
end;

{ TPDLBetween }

procedure TPDLBetween.Validate;
begin
  inherited Validate;
  if not Assigned(FValue1) then
    raise EPDLExceptionAnsi.Create(Self.ClassName + 'must have a value1');
  if not Assigned(FValue2) then
    raise EPDLExceptionAnsi.Create(Self.ClassName + 'must have a value2');
  if (LowerCase(FValue1.ToString)=LowerCase(FValue2)) then
    raise EPDLExceptionAnsi.Create('values of ' + Self.ClassName + 'must be different');
end;

constructor TPDLBetween.Create;
begin
  inherited Create;
  FValue1:=nil;
  FValue2:=nil;
end;

function TPDLBetween.Value1(AExpression: IPDLExpression): IPDLBetween;
begin
  FValue1=AExpression;
  Result:=Self;
end;

function TPDLBetween.Value2(AExpression: IPDLExpression): IPDLBetween;
begin
  FValue2:=AExpression;
  Result:=Self;
end;

function TPDLBetween.ToString: String;
begin
  Result:=' BETWEEN '
end;

{ TPDLLessEqualThan }

function TPDLLessEqualThan.ToString: String;
begin
  Result:=' <= ';
end;

{ TPDLLessThan }

function TPDLLessThan.ToString: String;
begin
  Result:=' < ';
end;

{ TPDLGreaterEqualThan }

function TPDLGreaterEqualThan.ToString: String;
begin
  Result:=' >= ';
end;

{ TPDLGreaterThan }

function TPDLGreaterThan.ToString: String;
begin
  Result:=' > ';
end;

{ TPDLLike }

function TPDLLike.ToString: String;
begin
  Result:=' LIKE ';
end;

{ TPDLDifferent }

function TPDLDifferent.ToString: String;
begin
  Result:=' <> ';
end;

{ TPDLEqual }

function TPDLEqual.ToString: String;
begin
  Result:=' = ';
end;

{ TPDLNotAnsi }

function TPDLNotAnsi.ToString: String;
begin
  Result:=' NOT ';
end;

{ TPDLOrAnsi }

function TPDLOrAnsi.ToString: String;
begin
  Result:=' OR ';
end;

{ TPDLAndAnsi }

function TPDLAndAnsi.ToString: String;
begin
  Result:=' AND ';
end;

{ TPDLLogicOperatorsAnsi }

function TPDLLogicOperatorsAnsi.PDLAnd: IPDLAnd;
begin
  Result:=TPDLAndAnsi.Create;
end;

function TPDLLogicOperatorsAnsi.PDLOr: IPDLOr;
begin
  Result:=TPDLOrAnsi.Create;
end;

function TPDLLogicOperatorsAnsi.PDLNot: IPDLNot;
begin
  Result:=TPDLNotAnsi.Create;
end;

{ TPDLBooleanExpressionAnsi }

constructor TPDLBooleanExpressionAnsi.Create(AExpression: IPDLExpression);
begin
  FExpressions:=TPDLExpressionList.Create;
  FExpressions.Add(AExpression);
end;

function TPDLBooleanExpressionAnsi.PDLAnd(AExpression: IPDLExpression
  ): IPDLBooleanExpression;
begin
  FExpressions.Add(TPDLAndAnsi.Create);
  FExpressions.Add(AExpression);
end;

function TPDLBooleanExpressionAnsi.PDLOr(AExpression: IPDLExpression
  ): IPDLBooleanExpression;
begin
  FExpressions.Add(TPDLOrAnsi.Create);
  FExpressions.Add(AExpression);
end;

function TPDLBooleanExpressionAnsi.ToString: String;
var
  I: Integer;
begin
  Result:=inherited ToString;
  for I:=0 to FExpressions.Count-1 do
    begin
      Result:=Result+FExpressions.ToString;
    end;
end;

{ TPDLBoolAnsi }

function TPDLBoolAnsi.PDLAnd(AExpr1: IPDLExpression; AExpr2: IPDLExpression
  ): IPDLBooleanExpression;
begin
  Result:=TPDLBooleanExpressionAnsi.Create(AExpr1).PDLAnd(AExpr2);
end;

function TPDLBoolAnsi.PDLOr(AExpr1: IPDLExpression; AExpr2: IPDLExpression
  ): IPDLBooleanExpression;
begin
  Result:=TPDLBooleanExpressionAnsi.Create(AExpr1).PDLOr(AExpr2);
end;

{ TPDLSQLAnsi }

function TPDLSQLAnsi.Bool: IPDLLanguageBool;
begin
  Result:=FBool;
end;

function TPDLSQLAnsi.Bool(AExpression: IPDLExpression
  ): IPDLBooleanExpression;
begin
  Result:=TPDLBooleanExpressionAnsi.Create(AExpression);
end;

function TPDLSQLAnsi.Commands: IPDLLanguageCommands;
begin
  Result:=FCommands;
end;

function TPDLSQLAnsi.Funcs: IPDLLanguageFuncs;
begin
  Result:=FFuncs;
end;

function TPDLSQLAnsi.GetName: String;
begin
  Result:='Ansi';
end;

constructor TPDLSQLAnsi.Create;
begin
  FLogic:=TPDLLogicOperatorsAnsi.Create;
  FComparer:=TPDLComparerOperatorsAnsi.Create;
  FBool:=TPDLBoolAnsi.Create;
  FCommands:=TPDLCommandsAnsi.Create;
  FFuncs:=TPDLLanguageFuncsAnsi.Create;
end;

destructor TPDLSQLAnsi.Destroy;
begin
  FLogic:=nil;
  FComparer:=nil;
  FBool:=nil;
  FCommands:=nil;
  FFuncs:=nil;
  inherited Destroy;
end;

function TPDLSQLAnsi.Logic: IPDLLanguageLogicOperators;
begin
  Result:=FLogic;
end;

function TPDLSQLAnsi.Comparer: IPDLLanguageComparerOperators;
begin
  Result:=FComparer;
end;

end.

