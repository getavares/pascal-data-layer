unit PDL.Builder.FB;

interface

uses
  PDL.Builder, PDL.Builder.Generic, System.SysUtils;

type
  TPDLFieldTypedFB = class(TPDLFieldTyped)
  protected
    function ToSQL: String; override;
  end;

  TPDLTableFB = class(TPDLTableGeneric)
  protected
    function InstantiateField: TPDLField; override;
  end;

  TPDLPlanFB = class(TPDLPlanGeneric)
  protected
    function InstantiateTable(State: TPDLTableState): IPDLTableStructure; override;
    function ExistTable(TableName: String): Boolean; override;
    function ExistField(TableName: String; FieldName: String): Boolean; override;
    function ExistIndex(IndexName: String): Boolean; override;
  end;

implementation

uses
  Data.PDL, FireDAC.Comp.Client;

{ TPDLFieldTypedFB }

function TPDLFieldTypedFB.ToSQL: String;
var
  NullableSQL: String;
  FieldTypeSQL: String;
begin
case FieldType of
  ftString, ftWideString: FieldTypeSQL:='VARCHAR('+IntToStr(Trunc(Size))+')';
  ftSmallint, ftWord, ftShortint, ftByte: FieldTypeSQL:='SMALLINT';
  ftInteger, ftAutoInc: FieldTypeSQL:='INTEGER';
  ftBoolean: FieldTypeSQL:='BOOLEAN';
  ftFloat: FieldTypeSQL:='DOUBLE PRECISION';
  ftCurrency, ftBCD, ftExtended, ftSingle:
    FieldTypeSQL:='NUMERIC('+FloatToStr(Size)+')';
  ftDate: FieldTypeSQL:='DATE';
  ftTime: FieldTypeSQL:='TIME';
  ftDateTime, ftTimeStamp: FieldTypeSQL:='TIMESTAMP';
  ftBytes, ftVarBytes, ftGraphic, ftBlob: FieldTypeSQL:='BLOB';
  ftMemo, ftFmtMemo, ftWideMemo: FieldTypeSQL:='BLOB SUB_TYPE TEXT';
  ftFixedChar, ftFixedWideChar: FieldTypeSQL:='CHAR('+IntToStr(Trunc(Size))+')';
  ftLargeint, ftLongWord: FieldTypeSQL:='BIGINT';
else
  FieldTypeSQL:='UNKNOW';
  end;
Result:=Name+' '+FieldTypeSQL;
if (not IsNullable) then
  NullableSQL:='NOT NULL';
if (not NullableSQL.IsEmpty) then
  Result:=Result+' '+NullableSQL;
end;

{ TPDLTableFB }

function TPDLTableFB.InstantiateField: TPDLField;
begin
Result:=TPDLFieldTypedFB.Create(Self);
end;

{ TPDLPlanFB }

function TPDLPlanFB.ExistField(TableName, FieldName: String): Boolean;
var
  qrExist: TFDQuery;
begin
qrExist:=TFDQuery.Create(Nil);
try
  qrExist.Connection:=Connection;
  qrExist.SQL.Add('SELECT * FROM '+TableName+' WHERE 1=0');
  qrExist.Open;
  Result:=Assigned(qrExist.FindField(FieldName));
  qrExist.Close;
finally
  qrExist.Free;
  end;
end;

function TPDLPlanFB.ExistIndex(IndexName: String): Boolean;
var
  qrExist: TFDQuery;
begin
qrExist:=TFDQuery.Create(Nil);
try
  qrExist.Connection:=Connection;
  qrExist.SQL.Add(
    'SELECT RPDL$INDEX_NAME FROM RPDL$INDICES '+
    'WHERE '+
    'RPDL$SYSTEM_FLAG=0 AND '+
    'RPDL$INDEX_NAME=:Name');
  qrExist.ParamByName('Name').AsString:=IndexName;
  qrExist.Open;
  Result:=qrExist.RecordCount>0;
  qrExist.Close;
finally
  qrExist.Free;
  end;
end;

function TPDLPlanFB.ExistTable(TableName: String): Boolean;
var
  qrExist: TFDQuery;
begin
qrExist:=TFDQuery.Create(Nil);
try
  qrExist.Connection:=Connection;
  qrExist.SQL.Add(
    'SELECT RPDL$RELATION_NAME FROM RPDL$RELATIONS '+
    'WHERE '+
    'RPDL$SYSTEM_FLAG=0 AND '+
    'RPDL$RELATION_NAME=:Name');
  qrExist.ParamByName('Name').AsString:=TableName;
  qrExist.Open;
  Result:=qrExist.RecordCount>0;
  qrExist.Close;
finally
  qrExist.Free;
  end;
end;

function TPDLPlanFB.InstantiateTable(State: TPDLTableState): IPDLTableStructure;
begin
Result:=TPDLTableFB.Create(State);
end;

end.
