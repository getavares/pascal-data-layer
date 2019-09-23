unit PDL.Builder.Generic;

interface

uses
  System.Classes, System.SysUtils, UPDLBuilder, System.StrUtils;

type
  TPDLFieldTypedGeneric = class(TPDLFieldTyped)
  protected
    function ToSQL: String; override;
  end;

  TPDLOrderedIndexGeneric = class(TPDLOrderedIndex)
  protected
    function ToSQL: String; override;
  end;

  TPDLForeignGeneric = class(TPDLForeign)
  protected
    function ToSQL: String; override;
  end;

  TPDLPrimaryGeneric = class(TPDLPrimary)
  protected
    function ToSQL: String; override;
  end;

  TPDLUniqueGeneric = class(TPDLUnique)
  protected
    function ToSQL: String; override;
  end;

  TPDLTableGeneric = class(TPDLTableStructure)
  protected
    function InstantiateField: TPDLField; override;
    function InstantiateIndex: TPDLOrderedIndex; override;
    function InstantiateForeign: TPDLForeign; override;
    function InstantiatePrimary: TPDLPrimary; override;
    function InstantiateUnique: TPDLUnique; override;
    function ToSQL: String; override;
  end;

  TPDLPlanGeneric = class(TPDLPlanSpecialized)
  protected
    function InstantiateTable(State: TPDLTableState): IPDLTableStructure; override;
    function ExistTable(TableName: String): Boolean; override; abstract;
    function ExistField(TableName: String; FieldName: String): Boolean; override; abstract;
    function ExistIndex(IndexName: String): Boolean; override; abstract;
  end;

implementation

uses
  Data.PDL;

{ TPDLOrderedIndexGeneric }

function TPDLOrderedIndexGeneric.ToSQL: String;
var
  FieldsSQL: String;
  Field: String;
  OrderOperator: String;
begin
FieldsSQL:=EmptyStr;
for Field in Fields do
  begin
  OrderOperator:=EmptyStr;
  if (not FieldsSQL.IsEmpty) then
    FieldsSQL:=FieldsSQL+',';
  FieldsSQL:=FieldsSQL+Field;
  if AscFields.IndexOf(Field)>-1 then
    OrderOperator:='asc'
  else if DescFields.IndexOf(Field)>-1 then
    OrderOperator:='desc';
  if (not OrderOperator.IsEmpty) then
    FieldsSQL:=FieldsSQL+' '+OrderOperator;
  end;
Result:='CREATE '+Name+' ON '+Table.Name+'('+FieldsSQL+')';
end;

{ TPDLForeignGeneric }

function TPDLForeignGeneric.ToSQL: String;
var
  FieldsSQL: String;
  ReferenceFieldsSQL: String;
  Field: String;
begin
for Field in Fields do
  begin
  if (not FieldsSQL.IsEmpty) then
    FieldsSQL:=FieldsSQL+',';
  FieldsSQL:=FieldsSQL+Field;
  end;
for Field in RefFields do
  begin
  if (not ReferenceFieldsSQL.IsEmpty) then
    ReferenceFieldsSQL:=ReferenceFieldsSQL+',';
  ReferenceFieldsSQL:=ReferenceFieldsSQL+Field;
  end;
Result:='CONSTRAINT '+Name+' FOREIGN KEY ('+FieldsSQL+') REFERENCES '+RefTable+' ('+ReferenceFieldsSQL+')';
end;

{ TPDLPrimaryGeneric }

function TPDLPrimaryGeneric.ToSQL: String;
var
  FieldsSQL: String;
  Field: String;
begin
for Field in Fields do
  begin
  if (not FieldsSQL.IsEmpty) then
    FieldsSQL:=FieldsSQL+',';
  FieldsSQL:=FieldsSQL+Field;
  end;
Result:='CONSTRAINT '+Name+' PRIMARY KEY ('+FieldsSQL+')';
end;

{ TPDLUniqueGeneric }

function TPDLUniqueGeneric.ToSQL: String;
var
  FieldsSQL: String;
  Field: String;
begin
for Field in Fields do
  begin
  if (not FieldsSQL.IsEmpty) then
    FieldsSQL:=FieldsSQL+',';
  FieldsSQL:=FieldsSQL+Field;
  end;
Result:='CONSTRAINT '+Name+' UNIQUE ('+FieldsSQL+')';
end;

{ TPDLFieldTypedGeneric }

function TPDLFieldTypedGeneric.ToSQL: String;
var
  NullableSQL: String;
  FieldTypeSQL: String;
begin
case FieldType of
  ftString: FieldTypeSQL:='VARCHAR('+Trunc(Size).ToString+')';
  ftSmallint: FieldTypeSQL:='SMALLINT';
  ftInteger: FieldTypeSQL:='INTEGER';
  ftWord: FieldTypeSQL:='WORD';
  ftBoolean: FieldTypeSQL:='BOOLEAN';
  ftFloat: FieldTypeSQL:='FLOAT';
  ftCurrency: FieldTypeSQL:='CURRENCY';
  ftBCD: FieldTypeSQL:='CURRENCY';
  ftDate: FieldTypeSQL:='DATE';
  ftTime: FieldTypeSQL:='TIME';
  ftDateTime: FieldTypeSQL:='DATETIME';
  ftBytes: FieldTypeSQL:='BLOB';
  ftVarBytes: FieldTypeSQL:='BLOB';
  ftAutoInc: FieldTypeSQL:='INTEGER';
  ftBlob: FieldTypeSQL:='BLOB';
  ftMemo: FieldTypeSQL:='BLOB';
  ftGraphic: FieldTypeSQL:='BLOB';
  ftFmtMemo: FieldTypeSQL:='BLOB';
  ftFixedChar: FieldTypeSQL:='CHAR('+Trunc(Size).ToString+')';
  ftWideString: FieldTypeSQL:='VARCHAR('+Trunc(Size).ToString+')';
  ftLargeint: FieldTypeSQL:='BIGINT';
  ftTimeStamp: FieldTypeSQL:='TIMESTAMP';
  ftFixedWideChar: FieldTypeSQL:='CHAR('+Trunc(Size).ToString+')';
  ftWideMemo: FieldTypeSQL:='BLOB';
  ftLongWord: FieldTypeSQL:='BIGWORD';
  ftShortint: FieldTypeSQL:='SHORTINT';
  ftByte: FieldTypeSQL:='BYTE';
  ftExtended: FieldTypeSQL:='DOUBLE';
  ftSingle: FieldTypeSQL:='DOUBLE';
else
  FieldTypeSQL:='UNKNOW';
  end;
Result:=Name+' '+FieldTypeSQL;
if (not IsNullable) then
  NullableSQL:='NOT NULL';
if (not NullableSQL.IsEmpty) then
  Result:=Result+' '+NullableSQL;
end;

{ TPDLTableGeneric }

function TPDLTableGeneric.InstantiateField: TPDLField;
begin
Result:=TPDLFieldTypedGeneric.Create(Self);
end;

function TPDLTableGeneric.InstantiateForeign: TPDLForeign;
begin
Result:=TPDLForeignGeneric.Create(Self);
end;

function TPDLTableGeneric.InstantiateIndex: TPDLOrderedIndex;
begin
Result:=TPDLOrderedIndexGeneric.Create(Self);
end;

function TPDLTableGeneric.InstantiatePrimary: TPDLPrimary;
begin
Result:=TPDLPrimaryGeneric.Create(Self);
end;

function TPDLTableGeneric.InstantiateUnique: TPDLUnique;
begin
Result:=TPDLUniqueGeneric.Create(Self);
end;

function TPDLTableGeneric.ToSQL: String;
var
  FieldsSQL: String;
  Field: IPDLField;
  Constraints: String;
  IndexesSQL: String;
  Index: IPDLIndex;

begin
if State<>tsDropping then
  begin
  for Field in Fields do
    if (not FieldsSQL.IsEmpty) then
      FieldsSQL:=String.Join(',', [FieldsSQL, IfThen(State=tsEditing, ' ADD COLUMN ', EmptyStr)+Field.ToSQL])
    else
      FieldsSQL:=Field.ToSQL;
  for Index in Indexes do
    begin
    if (Index is TPDLPrimary) or (Index is TPDLForeign) then
      begin
      if (not Constraints.IsEmpty) then
        Constraints:=String.Join(',', [Constraints, Index.ToSQL])
      else
        Constraints:=Index.ToSQL;
      end
    else
      begin
      if (not IndexesSQL.IsEmpty) then
        IndexesSQL:=String.Join(';', [IndexesSQL, Index.ToSQL])
      else
        IndexesSQL:=Index.ToSQL;
      end;
    end;
  if State=tsEditing then
    Result:='ALTER TABLE '+Name+' '+FieldsSQL
  else
    Result:='CREATE TABLE '+Name+' ('+FieldsSQL;
  if (not Constraints.IsEmpty) then
    Result:=Result+','+Constraints;
  Result:=Result+IfThen(State=tsEditing, EmptyStr, ')')+';';
  if (not IndexesSQL.IsEmpty) then
    Result:=Result+IndexesSQL+';';
  end
else
  Result:='DROP TABLE '+Name;
end;

{ TPDLPlanGeneric }

function TPDLPlanGeneric.InstantiateTable(State: TPDLTableState): IPDLTableStructure;
begin
Result:=TPDLTableGeneric.Create(State);
end;

end.
