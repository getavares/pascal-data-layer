unit PDL.Builder;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Math,
  Data.PDL,
  PDL.Connection;

type
  IPDLObject = interface
    function GetName: String;
    procedure SetName(const Value: String);
    property Name: String read GetName write SetName;
    function ToSQL: String;
  end;

  TPDLObject = class(TInterfacedObject, IPDLObject)
  private
    FName: String;
    function GetName: String;
    procedure SetName(const Value: String);
  protected
    function ToSQL: String; virtual; abstract;
  public
    constructor Create; virtual;
    property Name: String read GetName write SetName;
  end;

  IPDLTable = interface(IPDLObject)
  end;

  IPDLOwnedTable = interface
    function GetPDLTable: IPDLTable;
    property Table: IPDLTable read GetPDLTable;
  end;

  IPDLField = interface(IPDLObject)
    function Unsigned: IPDLField;
    function Nullable: IPDLField;
  end;

  IPDLFieldTyped = interface(IPDLField)
    function GetFieldType: TFieldType;
    procedure SetFieldType(const AFieldType: TFieldType);
    function GetSize: Double;
    procedure SetSize(const Value: Double);
    property FieldType: TFieldType read GetFieldType write SetFieldType;
    property Size: Double read GetSize write SetSize;
  end;

  TPDLField = class(TPDLObject, IPDLField, IPDLOwnedTable)
  private
    FUnsigned: Boolean;
    FNullable: Boolean;
    FTable: IPDLTable;
    function GetPDLTable: IPDLTable;
  protected
    function IsUnsigned: Boolean;
    function IsNullable: Boolean;
    property Table: IPDLTable read GetPDLTable;
  public
    constructor Create(ATable: IPDLTable); reintroduce;
    function Unsigned: IPDLField;
    function Nullable: IPDLField;
  end;

  TPDLFieldTyped = class(TPDLField, IPDLFieldTyped)
  private
    FFieldType: TFieldType;
    FSize: Double;
    function GetFieldType: TFieldType;
    function GetSize: Double;
    procedure SetFieldType(const Value: TFieldType);
    procedure SetSize(const Value: Double);
  public
    property FieldType: TFieldType read GetFieldType write SetFieldType;
    property Size: Double read GetSize write SetSize;
  end;

  IPDLIndex = interface(IPDLObject)
    function AddField(AFieldName: String): IPDLIndex;
  end;

  TPDLIndex = class(TPDLObject, IPDLIndex, IPDLOwnedTable)
  private
    FFields: TStringList;
    FTable: IPDLTable;
    function GetPDLTable: IPDLTable;
  protected
    property Fields: TStringList read FFields;
    property Table: IPDLTable read GetPDLTable;
  public
    constructor Create(ATable: IPDLTable); reintroduce; virtual;
    destructor Destroy; override;
    function AddField(AFieldName: String): IPDLIndex; virtual;
  end;

  TPDLForeignRule = (PDLfrCascade, PDLfrSetNull);

  IPDLForeign = interface(IPDLIndex)
    function AddReferenceField(AFieldName: String): IPDLForeign;
    function ReferenceTable(ATableName: String): IPDLForeign;
    function OnDelete(ARule: TPDLForeignRule): IPDLForeign;
    function OnUpdate(ARule: TPDLForeignRule): IPDLForeign;
    function AddField(AFieldName: String): IPDLForeign;
  end;

  TPDLForeign = class(TPDLIndex, IPDLForeign)
  private
    FReferenceFields: TStringList;
    FReferenceTable: String;
    FOnDelete: TPDLForeignRule;
    FOnUpdate: TPDLForeignRule;
  protected
    property RefFields: TStringList read FReferenceFields;
    property RefTable: String read FReferenceTable;
    property OnDeleteRule: TPDLForeignRule read FOnDelete;
    property OnUpdateRule: TPDLForeignRule read FOnUpdate;
  public
    constructor Create(ATable: IPDLTable); reintroduce;
    destructor Destroy; override;
    function AddField(AFieldName: String): IPDLForeign; reintroduce;
    function AddReferenceField(AFieldName: String): IPDLForeign;
    function ReferenceTable(ATableName: String): IPDLForeign;
    function OnDelete(ARule: TPDLForeignRule): IPDLForeign;
    function OnUpdate(ARule: TPDLForeignRule): IPDLForeign;
  end;

  IPDLPrimary = interface(IPDLIndex)
  end;

  TPDLPrimary = class(TPDLIndex, IPDLPrimary);

  IPDLUnique = interface(IPDLIndex)
  end;

  TPDLUnique = class(TPDLIndex, IPDLUnique);

  IPDLOrderedIndex = interface(IPDLIndex)
    function AddAscending(AFieldName: String): IPDLOrderedIndex;
    function AddDescending(AFieldName: String): IPDLOrderedIndex;
  end;

  TPDLOrderedIndex = class(TPDLIndex, IPDLOrderedIndex)
  private
    FAscending: TStringList;
    FDescending: TStringList;
  protected
    property AscFields: TStringList read FAscending;
    property DescFields: TStringList read FDescending;
  public
    constructor Create(ATable: IPDLTable); override;
    destructor Destroy; override;
    function AddAscending(AFieldName: String): IPDLOrderedIndex;
    function AddDescending(AFieldName: String): IPDLOrderedIndex;
  end;

  IPDLTableStructure = interface(IPDLTable)
    function AddIncrement(AName: String): IPDLField;
    function AdPDLigInteger(AName: String): IPDLField;
    function AdPDLinary(AName: String): IPDLField;
    function AdPDLoolean(AName: String): IPDLField;
    function AddChar(AName: String; Size: Integer): IPDLField;
    function AddDate(AName: String): IPDLField;
    function AddDateTime(AName: String): IPDLField;
    function AddDouble(AName: String; Size: Integer; Precision: Integer): IPDLField;
    function AddInteger(AName: String): IPDLField;
    function AddTime(AName: String): IPDLField;
    function AddTimeStamp(AName: String): IPDLField;
    function AddSmallInt(AName: String): IPDLField;
    function AddString(AName: String; Size: Integer): IPDLField;
    function AddIndex(AIndexName: String): IPDLOrderedIndex;
    function AddForeign(AForeignName: String): IPDLForeign;
    function AddPrimary(APrimaryName: String): IPDLPrimary;
    function AddUnique(AUniqueName: String): IPDLUnique;
  end;

  TPDLTable = class(TPDLObject, IPDLTable);

  TPDLTableState = (tsCreating, tsEditing, tsDropping);

  TPDLTableStructure = class(TPDLTable, IPDLTableStructure)
  private
    FFields: TList<IPDLField>;
    FIndexes: TList<IPDLIndex>;
    FState: TPDLTableState;
  protected
    property Fields: TList read FFields;
    property Indexes: TList read FIndexes;
    function InstantiateField: TPDLField; virtual; abstract;
    function InstantiateIndex: TPDLOrderedIndex; virtual; abstract;
    function InstantiateForeign: TPDLForeign; virtual; abstract;
    function InstantiatePrimary: TPDLPrimary; virtual; abstract;
    function InstantiateUnique: TPDLUnique; virtual; abstract;
    property State: TPDLTableState read FState;
  public
    constructor Create(AState: TPDLTableState); reintroduce;
    destructor Destroy; override;
    function AddIncrement(AName: String): IPDLField;
    function AdPDLigInteger(AName: String): IPDLField;
    function AdPDLinary(AName: String): IPDLField;
    function AdPDLoolean(AName: String): IPDLField;
    function AddChar(AName: String; Size: Integer): IPDLField;
    function AddDate(AName: String): IPDLField;
    function AddDateTime(AName: String): IPDLField;
    function AddDouble(AName: String; Size: Integer; Precision: Integer): IPDLField;
    function AddInteger(AName: String): IPDLField;
    function AddTime(AName: String): IPDLField;
    function AddTimeStamp(AName: String): IPDLField;
    function AddSmallInt(AName: String): IPDLField;
    function AddString(AName: String; Size: Integer): IPDLField;
    function AddIndex(AIndexName: String): IPDLOrderedIndex;
    function AddForeign(AForeignName: String): IPDLForeign;
    function AddPrimary(APrimaryName: String): IPDLPrimary;
    function AddUnique(AUniqueName: String): IPDLUnique;
  end;

  TPDLTableClass = class of TPDLTable;

  TPDLPlanProc = reference to procedure(Table: TPDLTableStructure);

  IPDLPlanStructure = interface
    procedure DropTable(TableName: String);
    function AlterTable(TableName: String): IPDLTableStructure;
    function CreateTable(TableName: String): IPDLTableStructure;
    function ExistTable(TableName: String): Boolean;
    function ExistField(TableName: String; FieldName: String): Boolean;
    function ExistIndex(IndexName: String): Boolean;
  end;

  IPDLPlan = interface(IPDLPlanStructure)
    procedure Build;
    procedure Unbuild;
  end;

  TPDLStrucurePlan = class(TInterfacedObject, IPDLPlanStructure)
  protected
    procedure DropTable(TableName: String); virtual; abstract;
    function AlterTable(TableName: String): IPDLTableStructure; virtual; abstract;
    function CreateTable(TableName: String): IPDLTableStructure; virtual; abstract;
    function ExistTable(TableName: String): Boolean; virtual; abstract;
    function ExistField(TableName: String; FieldName: String): Boolean; virtual; abstract;
    function ExistIndex(IndexName: String): Boolean; virtual; abstract;
  end;

  TPDLPlanSpecialized = class(TPDLStrucurePlan)
  private
    FTables: TInterfaceList;
    FConnection: TFDConnection;
    function GetTable(Index: Integer): TPDLTable;
    function GetTableCount: Integer;
  protected
    procedure DropTable(TableName: String); override;
    function AlterTable(TableName: String): IPDLTableStructure; override;
    function CreateTable(TableName: String): IPDLTableStructure; override;
    function InstantiateTable(State: TPDLTableState): IPDLTableStructure; virtual; abstract;
    function ExistTable(TableName: String): Boolean; override; abstract;
    function ExistField(TableName: String; FieldName: String): Boolean; override; abstract;
    function ExistIndex(IndexName: String): Boolean; override; abstract;
    procedure AddTable(ATable: TPDLTable);
    property Table[Index: Integer]: TPDLTable read GetTable;
    property TableCount: Integer read GetTableCount;
    property Connection: TFDConnection read FConnection;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPDLPlanSpecializedClass = class of TPDLPlanSpecialized;

  TPDLPlan = class(TPDLStrucurePlan, IPDLPlan)
  private
    FPDLPlanSpecialized: TPDLPlanSpecialized;
  public
    constructor Create(AConnection: TFDConnection;
      APDLSpecializedClass: TPDLPlanSpecializedClass); reintroduce;
    destructor Destroy; override;
    procedure Build; virtual; abstract;
    procedure Unbuild; virtual; abstract;
    procedure DropTable(TableName: String); override;
    function AlterTable(TableName: String): IPDLTableStructure; override;
    function CreateTable(TableName: String): IPDLTableStructure; override;
    function ExistTable(TableName: String): Boolean; override;
    function ExistField(TableName: String; FieldName: String): Boolean; override;
    function ExistIndex(IndexName: String): Boolean; override;
  end;

  TPDLPlanClass = class of TPDLPlan;

  TDatabaseBuilderPlanExecuted = reference to procedure(PlanClass: TPDLPlanClass);

  IPDLBuilder = interface
    procedure Build(PlanBuilded: TDatabaseBuilderPlanExecuted);
    procedure Unbuild(PlanUnBuilded: TDatabaseBuilderPlanExecuted);
  end;

  TPDLBuilderRegistration = class
  private
    FSpecializations: TDictionary<String, TPDLPlanSpecializedClass>;
    FPlans: TList;
    function GetPlansCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TPDLBuilderRegistration;
    procedure RegisterPlan(PlanClass: TPDLPlanClass);
    procedure RegisterSpecialization(DriverName: String; Specialization: TPDLPlanSpecializedClass);
    property PlansCount: Integer read GetPlansCount;
    class procedure Finalize;
  end;

  TPDLBuilder = class(TInterfacedObject, IPDLBuilder)
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    procedure Build(PlanBuilded: TDatabaseBuilderPlanExecuted);
    procedure Unbuild(PlanUnBuilded: TDatabaseBuilderPlanExecuted);
  end;

procedure DatabaseBuild(PlanBuilded: TDatabaseBuilderPlanExecuted);
procedure DatabaseUnBuild(PlanUnBuilded: TDatabaseBuilderPlanExecuted);

implementation

var
  PDLBuilderRegistration: TPDLBuilderRegistration = Nil;

procedure DatabaseBuild(PlanBuilded: TDatabaseBuilderPlanExecuted);
var
  PDLBuilder: TPDLBuilder;
  PDLConnection: TPDLConnection;
begin
PDLConnection:=TPDLConnection.Create;
PDLConnection.Connect;
PDLBuilder:=TPDLBuilder.Create(PDLConnection.Connection);
try
  PDLBuilder.Build(PlanBuilded);
finally
  PDLBuilder.Free;
  PDLConnection.Free;
  TPDLBuilderRegistration.Finalize;
  end;
end;

procedure DatabaseUnBuild(PlanUnBuilded: TDatabaseBuilderPlanExecuted);
var
  PDLBuilder: TPDLBuilder;
  PDLConnection: TPDLConnection;
begin
PDLConnection:=TPDLConnection.Create;
PDLBuilder:=TPDLBuilder.Create(PDLConnection.Connection);
try
  PDLBuilder.Unbuild(PlanUnBuilded);
finally
  PDLBuilder.Free;
  PDLConnection.Free;
  TPDLBuilderRegistration.Finalize;
  end;
end;

{ TPDLObject }

constructor TPDLObject.Create;
begin
FName:=EmptyStr;
end;

function TPDLObject.GetName: String;
begin
Result:=FName;
end;

procedure TPDLObject.SetName(const Value: String);
begin
if FName<>Value then
  FName:=Value;
end;

{ TPDLField }

constructor TPDLField.Create(ATable: IPDLTable);
begin
inherited Create;
FTable:=ATable;
FNullable:=False;
FUnsigned:=False;
end;

function TPDLField.GetPDLTable: IPDLTable;
begin
Result:=FTable;
end;

function TPDLField.IsNullable: Boolean;
begin
Result:=FNullable;
end;

function TPDLField.IsUnsigned: Boolean;
begin
Result:=FUnsigned;
end;

function TPDLField.Nullable: IPDLField;
begin
FNullable:=True;
Result:=Self;
end;

function TPDLField.Unsigned: IPDLField;
begin
FUnsigned:=True;
Result:=Self;
end;

{ TPDLIndex }

function TPDLIndex.AddField(AFieldName: String): IPDLIndex;
begin
if FFields.IndexOf(AFieldName)=-1 then
  FFields.Add(AFieldName);
Result:=Self;
end;

constructor TPDLIndex.Create(ATable: IPDLTable);
begin
inherited Create;
FTable:=ATable;
FFields:=TStringList.Create;
end;

destructor TPDLIndex.Destroy;
begin
FFields.Clear;
FFields.Free;
inherited;
end;

function TPDLIndex.GetPDLTable: IPDLTable;
begin
Result:=FTable;
end;

{ TPDLForeign }

function TPDLForeign.AddField(AFieldName: String): IPDLForeign;
begin
inherited AddField(AFieldName);
Result:=Self;
end;

function TPDLForeign.AddReferenceField(AFieldName: String): IPDLForeign;
begin
if FReferenceFields.IndexOf(AFieldName)=-1 then
  FReferenceFields.Add(AFieldName);
Result:=Self;
end;

constructor TPDLForeign.Create(ATable: IPDLTable);
begin
inherited Create(ATable);
FReferenceFields:=TStringList.Create;
FReferenceTable:=EmptyStr;
FOnDelete:=PDLfrCascade;
FOnUpdate:=PDLfrCascade;
end;

destructor TPDLForeign.Destroy;
begin
FReferenceFields.Clear;
FReferenceFields.Free;
inherited;
end;

function TPDLForeign.OnDelete(ARule: TPDLForeignRule): IPDLForeign;
begin
FOnDelete:=ARule;
Result:=Self;
end;

function TPDLForeign.OnUpdate(ARule: TPDLForeignRule): IPDLForeign;
begin
FOnUpdate:=ARule;
Result:=Self;
end;

function TPDLForeign.ReferenceTable(ATableName: String): IPDLForeign;
begin
FReferenceTable:=ATableName;
Result:=Self;
end;

{ TPDLOrderedIndex }

function TPDLOrderedIndex.AddAscending(AFieldName: String): IPDLOrderedIndex;
var
  DescIndex: Integer;
begin
AddField(AFieldName);
DescIndex:=FDescending.IndexOf(AFieldName);
if DescIndex>-1 then
  FDescending.Delete(DescIndex);
FAscending.Add(AFieldName);
Result:=Self;
end;

function TPDLOrderedIndex.AddDescending(AFieldName: String): IPDLOrderedIndex;
var
  AscIndex: Integer;
begin
AddField(AFieldName);
AscIndex:=FAscending.IndexOf(AFieldName);
if AscIndex>-1 then
  FAscending.Delete(AscIndex);
FDescending.Add(AFieldName);
Result:=Self;
end;

constructor TPDLOrderedIndex.Create(ATable: IPDLTable);
begin
inherited;
FAscending:=TStringList.Create;
FDescending:=TStringList.Create;
end;

destructor TPDLOrderedIndex.Destroy;
begin
FAscending.Clear;
FDescending.Clear;
FAscending.Free;
FDescending.Free;
inherited;
end;

{ TPDLTable }

function TPDLTableStructure.AdPDLigInteger(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftInteger;
FFields.Add(Result);
end;

function TPDLTableStructure.AdPDLinary(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftBlob;
FFields.Add(Result);
end;

function TPDLTableStructure.AdPDLoolean(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftBoolean;
FFields.Add(Result);
end;

function TPDLTableStructure.AddChar(AName: String; Size: Integer): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftFixedChar;
TPDLFieldTyped(Result).Size:=Size;
FFields.Add(Result);
end;

function TPDLTableStructure.AddDate(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftDate;
FFields.Add(Result);
end;

function TPDLTableStructure.AddDateTime(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftDateTime;
FFields.Add(Result);
end;

function TPDLTableStructure.AddDouble(AName: String; Size, Precision: Integer): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftFloat;
TPDLFieldTyped(Result).Size:=StrToFloat(Size.ToString+FormatSettings.DecimalSeparator+Precision.ToString);
FFields.Add(Result);
end;

function TPDLTableStructure.AddForeign(AForeignName: String): IPDLForeign;
begin
Result:=InstantiateForeign;
Result.Name:=AForeignName;
FIndexes.Add(Result);
end;

function TPDLTableStructure.AddIncrement(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftInteger;
FFields.Add(Result);
end;

function TPDLTableStructure.AddIndex(AIndexName: String): IPDLOrderedIndex;
begin
Result:=InstantiateIndex;
Result.Name:=AIndexName;
FIndexes.Add(Result);
end;

function TPDLTableStructure.AddInteger(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftInteger;
FFields.Add(Result);
end;

function TPDLTableStructure.AddPrimary(APrimaryName: String): IPDLPrimary;
begin
Result:=InstantiatePrimary;
Result.Name:=APrimaryName;
FIndexes.Add(Result);
end;

function TPDLTableStructure.AddSmallInt(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftSmallint;
FFields.Add(Result);
end;

function TPDLTableStructure.AddString(AName: String; Size: Integer): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftString;
TPDLFieldTyped(Result).Size:=Size;
FFields.Add(Result);
end;

function TPDLTableStructure.AddTime(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftTime;
FFields.Add(Result);
end;

function TPDLTableStructure.AddTimeStamp(AName: String): IPDLField;
begin
Result:=InstantiateField;
Result.Name:=AName;
TPDLFieldTyped(Result).FieldType:=ftTimeStamp;
FFields.Add(Result);
end;

function TPDLTableStructure.AddUnique(AUniqueName: String): IPDLUnique;
begin
Result:=InstantiateUnique;
Result.Name:=AUniqueName;
FIndexes.Add(Result);
end;

constructor TPDLTableStructure.Create(AState: TPDLTableState);
begin
inherited Create;
FState:=AState;
FFields:=TList<IPDLField>.Create;
FIndexes:=TList<IPDLIndex>.Create;
end;

destructor TPDLTableStructure.Destroy;
begin
FFields.Clear;
FFields.Free;
FIndexes.Clear;
FIndexes.Free;
inherited;
end;

{ TPDLFieldTyped }

function TPDLFieldTyped.GetFieldType: TFieldType;
begin
Result:=FFieldType;
end;

function TPDLFieldTyped.GetSize: Double;
begin
Result:=FSize;
end;

procedure TPDLFieldTyped.SetFieldType(const Value: TFieldType);
begin
if FFieldType<>Value then
  FFieldType:=Value;
end;

procedure TPDLFieldTyped.SetSize(const Value: Double);
begin
if FSize<>Value then
  FSize:=Value;
end;

{ TPDLPlan }

function TPDLPlan.AlterTable(TableName: String): IPDLTableStructure;
begin
Result:=FPDLPlanSpecialized.AlterTable(TableName);
end;

constructor TPDLPlan.Create(AConnection: TFDConnection; APDLSpecializedClass: TPDLPlanSpecializedClass);
begin
FPDLPlanSpecialized:=APDLSpecializedClass.Create;
FPDLPlanSpecialized.FConnection:=AConnection;
end;

function TPDLPlan.CreateTable(TableName: String): IPDLTableStructure;
begin
Result:=FPDLPlanSpecialized.CreateTable(TableName);
end;

destructor TPDLPlan.Destroy;
begin
FPDLPlanSpecialized.Free;
inherited;
end;

procedure TPDLPlan.DropTable(TableName: String);
begin
FPDLPlanSpecialized.DropTable(TableName);
end;

function TPDLPlan.ExistField(TableName, FieldName: String): Boolean;
begin
Result:=FPDLPlanSpecialized.ExistField(TableName, FieldName);
end;

function TPDLPlan.ExistIndex(IndexName: String): Boolean;
begin
Result:=FPDLPlanSpecialized.ExistIndex(IndexName);
end;

function TPDLPlan.ExistTable(TableName: String): Boolean;
begin
Result:=FPDLPlanSpecialized.ExistTable(TableName);
end;

{ TPDLPlanSpecialized }

procedure TPDLPlanSpecialized.AddTable(ATable: TPDLTable);
begin
if FTables.IndexOf(ATable)=-1 then
  FTables.Add(ATable);
end;

function TPDLPlanSpecialized.AlterTable(TableName: String): IPDLTableStructure;
var
  Table: IPDLTableStructure;
begin
Table:=InstantiateTable(tsEditing);
Table.Name:=TableName;
FTables.Add(TPDLTable(Table));
Result:=Table;
end;

constructor TPDLPlanSpecialized.Create;
begin
inherited;
FConnection:=Nil;
FTables:=TInterfaceList.Create;
end;

function TPDLPlanSpecialized.CreateTable(TableName: String): IPDLTableStructure;
var
  Table: IPDLTableStructure;
begin
Table:=InstantiateTable(tsCreating);
Table.Name:=TableName;
FTables.Add(TPDLTable(Table));
Result:=Table;
end;

destructor TPDLPlanSpecialized.Destroy;
begin
FTables.Clear;
FTables.Free;
inherited;
end;

procedure TPDLPlanSpecialized.DropTable(TableName: String);
var
  Table: IPDLTableStructure;
begin
Table:=InstantiateTable(tsDropping);
Table.Name:=TableName;
FTables.Add(TPDLTable(Table));
end;

function TPDLPlanSpecialized.GetTable(Index: Integer): TPDLTable;
begin
if FTables.Count>0 then
  Result:=TPDLTable(FTables[Index])
else
  Result:=Nil;
end;

function TPDLPlanSpecialized.GetTableCount: Integer;
begin
Result:=FTables.Count;
end;

{ TDatabaseBuilder }

procedure TPDLBuilder.Build(PlanBuilded: TDatabaseBuilderPlanExecuted);
var
  PlanClass: TPDLPlanClass;
  ExistPlan: Boolean;
  qrPlans: TFDQuery;
  qrBuild: TFDQuery;
  qrRegister: TFDQuery;
  Plan: TPDLPlan;
  Table: TPDLTable;
  I: Integer;
begin
qrPlans:=TFDQuery.Create(Nil);
try
  qrPlans.Connection:=FConnection;
  with qrPlans.SQL do
    Add('SELECT COUNT(*) AS PlanCount FROM Structure WHERE PlanName=:PlanName');
  for PlanClass in TPDLBuilderRegistration.Instance.FPlans do
    begin
    if qrPlans.Active then
      qrPlans.Close;
    qrPlans.ParamByName('PlanName').AsString:=PlanClass.ClassName;
    qrPlans.Open;
    ExistPlan:=qrPlans.FielPDLyName('PlanCount').AsInteger>0;
    qrPlans.Close;
    if (not ExistPlan) then
      begin
      Plan:=PlanClass.Create(FConnection,
        TPDLBuilderRegistration.Instance.FSpecializations.Items[FConnection.DriverName]);
      try
        Plan.Build;
        qrBuild:=TFDQuery.Create(Nil);
        try
          qrBuild.Connection:=FConnection;
          for I:=0 to Plan.FPDLPlanSpecialized.TableCount-1 do
            begin
            Table:=Plan.FPDLPlanSpecialized.Table[I];
            qrBuild.SQL.Add(Table.ToSQL);
            end;
          try
            FConnection.StartTransaction;
            qrBuild.ExecSQL;
            qrRegister:=TFDQuery.Create(Nil);
            try
              if Assigned(PlanBuilded) then
                PlanBuilded(PlanClass);
              qrRegister.Connection:=FConnection;
              qrRegister.SQL.Add('INSERT INTO Structure (PlanName, BuildDate) VALUES (:PlanName, :BuildDate)');
              qrRegister.ParamByName('PlanName').AsString:=PlanClass.ClassName;
              qrRegister.ParamByName('BuildDate').AsDateTime:=Now;
              qrRegister.ExecSQL;
            finally
              qrRegister.Free;
            end;
            FConnection.Commit;
          except
            FConnection.Rollback;
            raise;
          end;
        finally
          qrBuild.Free;
          end;
      finally
        Plan.Free;
        end;
      end;
    end;
finally
  qrPlans.Free;
  end;
end;

constructor TPDLBuilder.Create(AConnection: TFDConnection);
begin
FConnection:=AConnection;
end;

procedure TPDLBuilder.Unbuild(PlanUnBuilded: TDatabaseBuilderPlanExecuted);
var
  PlanClass: TPDLPlanClass;
  qrPlans: TFDQuery;
  qrBuild: TFDQuery;
  qrRegister: TFDQuery;
  Plan: TPDLPlan;
  Table: TPDLTable;
  PlanExist: Boolean;
  I: Integer;
begin
qrPlans:=TFDQuery.Create(Nil);
try
  qrPlans.Connection:=FConnection;
  with qrPlans.SQL do
    Add('SELECT COUNT(*) AS PlanCount FROM Structure WHERE PlanName=:PlanName');
  if TPDLBuilderRegistration.Instance.FPlans.Count=0 then
    raise Exception.Create('PDLBuilder has no Plan registrated to unbuild');
  I:=TPDLBuilderRegistration.Instance.PlansCount-1;
  repeat
    if qrPlans.Active then
      qrPlans.Close;
    PlanClass:=TPDLBuilderRegistration.Instance.FPlans[I];
    qrPlans.ParamByName('PlanName').AsString:=PlanClass.ClassName;
    qrPlans.Open;
    PlanExist:=qrPlans.FielPDLyName('PlanCount').AsInteger>0;
    qrPlans.Close;
    Dec(I);
  until PlanExist or (I<=0);
  if PlanExist then
    begin
    Plan:=PlanClass.Create(FConnection,
      TPDLBuilderRegistration.Instance.FSpecializations.Items[FConnection.DriverName]);
    try
      Plan.Unbuild;
      qrBuild:=TFDQuery.Create(Nil);
      try
        qrBuild.Connection:=FConnection;
        for I:=0 to Plan.FPDLPlanSpecialized.TableCount-1 do
          begin
          Table:=Plan.FPDLPlanSpecialized.Table[I];
          qrBuild.SQL.Add(Table.ToSQL);
          end;
        qrBuild.ExecSQL;
        qrRegister:=TFDQuery.Create(Nil);
        try
          if Assigned(PlanUnBuilded) then
            PlanUnBuilded(PlanClass);
          qrRegister.Connection:=FConnection;
          qrRegister.SQL.Add('DELETE FROM Structure WHERE PlanName=:PlanName');
          qrRegister.ParamByName('PlanName').AsString:=PlanClass.ClassName;
          qrRegister.ExecSQL;
        finally
          qrRegister.Free;
        end;
      finally
        qrBuild.Free;
        end;
    finally
      Plan.Free;
      end;
    end
  else
    raise Exception.Create('PDLBuilder no find Plan registrated to unbuild');
finally
  qrPlans.Free;
  end;
end;

{ TPDLBuilderRegistration }

constructor TPDLBuilderRegistration.Create;
begin
FSpecializations:=TDictionary<String, TPDLPlanSpecializedClass>.Create;
FPlans:=TList<TPDLPlanClass>.Create;
end;

destructor TPDLBuilderRegistration.Destroy;
begin
FSpecializations.Clear;
FSpecializations.Free;
FPlans.Clear;
FPlans.Free;
inherited;
end;

class procedure TPDLBuilderRegistration.Finalize;
begin
if Assigned(PDLBuilderRegistration) then
  FreeAndNil(PDLBuilderRegistration);
end;

function TPDLBuilderRegistration.GetPlansCount: Integer;
begin
Result:=FPlans.Count;
end;

class function TPDLBuilderRegistration.Instance: TPDLBuilderRegistration;
begin
if (not Assigned(PDLBuilderRegistration)) then
  PDLBuilderRegistration:=TPDLBuilderRegistration.Create;
Result:=PDLBuilderRegistration;
end;

procedure TPDLBuilderRegistration.RegisterPlan(PlanClass: TPDLPlanClass);
begin
if (not FPlans.Contains(PlanClass)) then
  FPlans.Add(PlanClass);
end;

procedure TPDLBuilderRegistration.RegisterSpecialization(
  DriverName: String; Specialization: TPDLPlanSpecializedClass);
begin
if (not FSpecializations.ContainsKey(DriverName)) then
  FSpecializations.Add(DriverName, Specialization);
end;

end.
