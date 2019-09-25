unit PDL.Connection;

interface

uses
  Classes, SysUtils;

type
  IPDLConnection = interface
    procedure Connect;
    procedure Disconnect;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
  end;

  { TPDLGenericConnection }

  TPDLGenericConnection = class(TInterfacedObject, IPDLConnection)
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
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
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
    procedure Load(AParamsPath: String);
    property Params: TStringList read FParams;
    property ConnectionClass: TPDLGenericConnectionClass read GetConnectionClass;
  end;

  TPDLConnection = class
  private
    FPDLConnection: IPDLConnection;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
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
FPDLConnection.Commit;
end;

procedure TPDLConnection.Connect;
begin
FPDLConnection.Connect;
//TPDLFuncs.Initialize(FPDLConnection.FConnection.DriverName);
end;

constructor TPDLConnection.Create;
begin
FPDLConnection:=TPDLConnectionParams.Instance.ConnectionClass.Create;
end;

destructor TPDLConnection.Destroy;
begin
if Assigned(FPDLConnection) then
  FPDLConnection:=nil;
inherited;
end;

procedure TPDLConnection.Disconnect;
begin
FPDLConnection.Disconnect;
end;

procedure TPDLConnection.Rollback;
begin
FPDLConnection.Rollback;
end;

procedure TPDLConnection.StartTransaction;
begin
FPDLConnection.StartTransaction;
end;

{ TPDLGenericConnection }

procedure TPDLGenericConnection.Connect;
begin
TPDLLayer.Instance.Connect(FConnection);
end;

constructor TPDLGenericConnection.Create;
begin
FParams:=TStringList.Create;
LoadParams(TPDLConnectionParams.Instance.Params);
end;

destructor TPDLGenericConnection.Destroy;
begin
Disconnect;
FConnection.Free;
FParams.Clear;
FParams.Free;
inherited;
end;

procedure TPDLGenericConnection.Disconnect;
begin
TPDLLayer.Instance.Disconnect(FConnection);
end;

procedure TPDLGenericConnection.StartTransaction;
begin
TPDLLayer.Instance.StartTransaction(FConnection);
end;

procedure TPDLGenericConnection.Commit;
begin
TPDLLayer.Instance.Commit(FConnection);
end;

procedure TPDLGenericConnection.Rollback;
begin
TPDLLayer.Instance.Rollback(FConnection);
end;

procedure TPDLGenericConnection.LoadParams(AParams: TStringList);
begin
FParams.Assign(AParams);
AssignParams;
end;

{ TPDLConnectionParams }

constructor TPDLConnectionParams.Create;
begin
FParams:=TStringList.Create;
FPDLSpecializations:=TStringList.Create;
end;

destructor TPDLConnectionParams.Destroy;
begin
FParams.Free;
FPDLSpecializations.Clear;
FPDLSpecializations.Free;
inherited;
end;

function TPDLConnectionParams.GetConnectionClass: TPDLGenericConnectionClass;
var
  SpecIndex: Integer;
  DriverName: String;
begin
DriverName:=FParams.Values['DriverName'];
SpecIndex:=FPDLSpecializations.IndexOf(DriverName);
if SpecIndex=-1 then
  raise Exception.Create(Format('Connection specialization of %s not found', [DriverName]));
Result:=TPDLGenericConnectionClass(FPDLSpecializations.Objects[SpecIndex]);
end;

class function TPDLConnectionParams.Instance: TPDLConnectionParams;
begin
if not Assigned(PDLConnectionParams) then
  PDLConnectionParams:=TPDLConnectionParams.Create;
Result:=PDLConnectionParams;
end;

procedure TPDLConnectionParams.Load(AParamsPath: String);
begin
FParams.LoadFromFile(AParamsPath);
end;

procedure TPDLConnectionParams.RegisterSpecialization(DriverName: String;
  Specialization: TPDLGenericConnectionClass);
begin
if (FPDLSpecializations.IndexOf(DriverName)=-1) then
  FPDLSpecializations.AddObject(DriverName, TObject(Specialization));
end;

end.
