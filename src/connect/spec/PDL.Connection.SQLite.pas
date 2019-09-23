unit UPDLSQLiteConnection;

interface

uses
  System.SysUtils,
  Data.PDL,
  PDL.Connection;

type
  TPDLConnectionSQLite = class(TPDLGenericConnection)
  private
    FCreateIfNotExists: Boolean;
    procedure AfterConnect(Sender: TObject);
    procedure BeforeConnect(Sender: TObject);
  protected
    procedure AssignParams; override;
  public
    constructor Create; override;
    property CreateIfNotExists: Boolean read FCreateIfNotExists write FCreateIfNotExists;
  end;

implementation


{ TPDLConnectionSQLite }

constructor TPDLConnectionSQLite.Create;
begin
inherited;
FCreateIfNotExists:=True;
Connection.AfterConnect:=AfterConnect;
Connection.BeforeConnect:=BeforeConnect;
end;

procedure TPDLConnectionSQLite.AfterConnect(Sender: TObject);
begin
if Connection.Params.Values['OpenMode']='CreateUTF16' then
  begin
  Connection.Close;
  Connection.Open;
  end;
end;

procedure TPDLConnectionSQLite.AssignParams;
begin
inherited;
Connection.Params.Database:=Params.Values['Database'];
end;

procedure TPDLConnectionSQLite.BeforeConnect(Sender: TObject);
begin
if FCreateIfNotExists and (not FileExists(Connection.Params.Database)) then
  Connection.Params.Values['OpenMode']:='CreateUTF16'
else
  Connection.Params.Values['OpenMode']:='ReadWrite';
end;

initialization

TPDLConnectionParams.Instance.RegisterSpecialization('SQLite', TPDLConnectionSQLite);

end.
