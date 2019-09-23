unit PDL.Connection.FB;

interface

uses
  System.SysUtils, Data.PDL, PDL.Connection;

type
  TPDLConnectionFB = class(TPDLGenericConnection)
  protected
    procedure AssignParams; override;
  end;

implementation


{ TPDLConnectionFB }

procedure TPDLConnectionFB.AssignParams;
begin
inherited;
Connection.Params.Values['Protocol']:='TCPIP';
Connection.Params.Database:=Params.Values['Database'];
Connection.Params.UserName:=Params.Values['UserName'];
Connection.Params.Password:=Params.Values['Password'];
Connection.Params.Values['Server']:=Params.Values['Server'];
Connection.Params.Values['Port']:=Params.Values['Port'];
end;

initialization

TPDLConnectionParams.Instance.RegisterSpecialization('FB', TPDLConnectionFB);

end.
