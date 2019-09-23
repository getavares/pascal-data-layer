unit PDL.Builder.SQLite;

interface

uses
  System.SysUtils, PDL.Builder, PDL.Builder.Generic;

type
  TPDLFieldTypedSQLite = class(TPDLFieldTyped)
  protected
    function ToSQL: String; override;
  end;

  TPDLTableSQLite = class(TPDLTableGeneric)
  protected
    function InstantiateField: TPDLField; override;
  end;

  TPDLPlanSQLite = class(TPDLPlanGeneric)
  private
    function ExistStructure(StructureType: String; StructureName: String): Boolean;
  protected
    function InstantiateTable(State: TPDLTableState): IPDLTableStructure; override;
    function ExistTable(TableName: String): Boolean; override;
    function ExistField(TableName: String; FieldName: String): Boolean; override;
    function ExistIndex(IndexName: String): Boolean; override;
  end;

implementation

uses
  Data.PDL, FireDAC.Comp.Client;

{ TPDLFieldTypedSQLite }

function TPDLFieldTypedSQLite.ToSQL: String;
var
  NullableSQL: String;
  FieldTypeSQL: String;
begin
case FieldType of
  ftString: FieldTypeSQL:='TEXT';
  ftSmallint: FieldTypeSQL:='INTEGER';
  ftInteger: FieldTypeSQL:='INTEGER';
  ftWord: FieldTypeSQL:='INTEGER';
  ftBoolean: FieldTypeSQL:='INTEGER';
  ftFloat: FieldTypeSQL:='REAL';
  ftCurrency: FieldTypeSQL:='REAL';
  ftBCD: FieldTypeSQL:='REAL';
  ftDate: FieldTypeSQL:='TEXT';
  ftTime: FieldTypeSQL:='TEXT';
  ftDateTime: FieldTypeSQL:='TEXT';
  ftBytes: FieldTypeSQL:='BLOB';
  ftVarBytes: FieldTypeSQL:='BLOB';
  ftAutoInc: FieldTypeSQL:='INTEGER';
  ftBlob: FieldTypeSQL:='BLOB';
  ftMemo: FieldTypeSQL:='BLOB';
  ftGraphic: FieldTypeSQL:='BLOB';
  ftFmtMemo: FieldTypeSQL:='BLOB';
  ftFixedChar: FieldTypeSQL:='TEXT';
  ftWideString: FieldTypeSQL:='TEXT';
  ftLargeint: FieldTypeSQL:='INTEGER';
  ftTimeStamp: FieldTypeSQL:='TEXT';
  ftFixedWideChar: FieldTypeSQL:='TEXT';
  ftWideMemo: FieldTypeSQL:='BLOB';
  ftLongWord: FieldTypeSQL:='INTEGER';
  ftShortint: FieldTypeSQL:='INTEGER';
  ftByte: FieldTypeSQL:='BLOB';
  ftExtended: FieldTypeSQL:='REAL';
  ftSingle: FieldTypeSQL:='REAL';
else
  FieldTypeSQL:='UNKNOW';
  end;
Result:=Name+' '+FieldTypeSQL;
if (not IsNullable) then
  NullableSQL:='NOT NULL';
if (not NullableSQL.IsEmpty) then
  Result:=Result+' '+NullableSQL;
end;

{ TPDLTableSQLite }

function TPDLTableSQLite.InstantiateField: TPDLField;
begin
Result:=TPDLFieldTypedSQLite.Create(Self);
end;

{ TPDLPlanSQLite }

function TPDLPlanSQLite.ExistField(TableName, FieldName: String): Boolean;
var
  qrExist: TFDQuery;
begin
qrExist:=TFDQuery.Create(Nil);
try
  qrExist.SQL.Add('SELECT * FROM '+TableName+' WHERE 1=0');
  qrExist.Open;
  Result:=Assigned(qrExist.FindField(FieldName));
  qrExist.Close;
finally
  qrExist.Free;
  end;
end;

function TPDLPlanSQLite.ExistIndex(IndexName: String): Boolean;
begin
Result:=ExistStructure('index', IndexName);
end;

function TPDLPlanSQLite.ExistStructure(StructureType,
  StructureName: String): Boolean;
var
  qrExist: TFDQuery;
begin
qrExist:=TFDQuery.Create(Nil);
try
  qrExist.SQL.Add('SELECT name FROM sqlite_master WHERE type=:Type AND name=:Name');
  qrExist.ParamByName('Type').AsString:=StructureType;
  qrExist.ParamByName('Name').AsString:=StructureName;
  qrExist.Open;
  Result:=qrExist.RecordCount>0;
  qrExist.Close;
finally
  qrExist.Free;
  end;
end;

function TPDLPlanSQLite.ExistTable(TableName: String): Boolean;
begin
Result:=ExistStructure('table', TableName);
end;

function TPDLPlanSQLite.InstantiateTable(State: TPDLTableState): IPDLTableStructure;
begin
Result:=TPDLTableSQLite.Create(State);
end;

initialization

TPDLBuilderRegistration.Instance.RegisterSpecialization('SQLite', TPDLPlanSQLite);

end.
