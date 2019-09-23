unit PDL.Connection;

interface

uses
  Classes;

type
  TPDLParamsStoreType = (pstFile, pstWindowsRegistry);

  TPDLGenericConnection = class
  private
    FParams: TStringList;
    FConnection: TObject;
    procedure LoadParams(AParams: TStringList);
  protected
    procedure AssignParams; virtual; abstract;
    property Params: TStringList read FParams;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    property Connection: TObject read FConnection;
  end;

  TPDLGenericConnectionClass = class of TPDLGenericConnection;

  TPDLConnectionParams = class
  private
    FParams: TStringList;
    FPDLSpecializations: TStringList;
    function GetConnectionClass: TPDLGenericConnectionClass;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TPDLConnectionParams;
    procedure RegisterSpecialization(DriverName: String;
      Specialization: TPDLGenericConnectionClass);
    procedure Load(AParamsPath: String; AStoreType: TPDLParamsStoreType);
    property Params: TStringList read FParams;
    property ConnectionClass: TPDLGenericConnectionClass read GetConnectionClass;
  end;

  TPDLConnection = class
  private
    FPDLConnection: TPDLGenericConnection;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function Connection: TObject;
    procedure Commit;
    procedure Rollback;
    procedure StartTransaction;
  end;

implementation

uses
  PDL.Layer;

var
  PDLConnectionParams: TPDLConnectionParams;

{ TPDLConnection }

procedure TPDLConnection.Commit;
begin
TPDLLayer.Instance.Commit(FPDLConnection.FConnection);
end;

procedure TPDLConnection.Connect;
begin
FPDLConnection.Connect;
TPDLFuncs.Initialize(FPDLConnection.FConnection.DriverName);
end;

function TPDLConnection.Connection: TFDConnection;
begin
Result:=FPDLConnection.Connection;
end;

constructor TPDLConnection.Create;
begin
FPDLConnection:=TPDLConnectionParams.Instance.ConnectionClass.Create;
end;

destructor TPDLConnection.Destroy;
begin
if Assigned(FPDLConnection) then
  begin
  FPDLConnection.Free;
  end;
inherited;
end;

procedure TPDLConnection.Disconnect;
begin
FPDLConnection.Disconnect;
end;

procedure TPDLConnection.Rollback;
begin
Connection.Rollback;
end;

procedure TPDLConnection.StartTransaction;
begin
Connection.StartTransaction;
end;

{ TPDLGenericConnection }

procedure TPDLGenericConnection.Connect;
begin
FConnection.Open;
end;

constructor TPDLGenericConnection.Create;
begin
FParams:=TStringList.Create;
FConnection:=TFDConnection.Create(Nil);
LoadParams(TPDLConnectionParams.Instance.Params);
end;

destructor TPDLGenericConnection.Destroy;
begin
FConnection.Close;
FConnection.Free;
FParams.Clear;
FParams.Free;
inherited;
end;

procedure TPDLGenericConnection.Disconnect;
begin
FConnection.Close;
end;

procedure TPDLGenericConnection.LoadParams(AParams: TStringList);
begin
FParams.Assign(AParams);
FConnection.DriverName:=FParams.Values['DriverName'];
AssignParams;
end;

{ TPDLConnectionParams }

constructor TPDLConnectionParams.Create;
begin
FParams:=TStringList.Create;
FPDLSpecializations:=TDictionary<String,TPDLGenericConnectionClass>.Create;
end;

destructor TPDLConnectionParams.Destroy;
begin
FParams.Free;
FPDLSpecializations.Clear;
FPDLSpecializations.Free;
inherited;
end;

function TPDLConnectionParams.GetConnectionClass: TPDLGenericConnectionClass;
begin
Result:=FPDLSpecializations.Items[FParams.Values['DriverName']];
end;

class function TPDLConnectionParams.Instance: TPDLConnectionParams;
begin
if not Assigned(PDLConnectionParams) then
  PDLConnectionParams:=TPDLConnectionParams.Create;
Result:=PDLConnectionParams;
end;

procedure TPDLConnectionParams.Load(AParamsPath: String;
  AStoreType: TPDLCParamsStoreType);
begin
if AStoreType=pstFile then
  FParams.LoadFromFile(AParamsPath);
end;

procedure TPDLConnectionParams.RegisterSpecialization(DriverName: String;
  Specialization: TPDLGenericConnectionClass);
begin
if (not FPDLSpecializations.ContainsKey(DriverName)) then
  FPDLSpecializations.Add(DriverName, Specialization);
end;

end.
