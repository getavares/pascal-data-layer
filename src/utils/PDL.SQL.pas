unit PDL.Funcs;

interface

uses
  SysUtils;

type
  TPDLFuncs = class sealed
  public
    class procedure Initialize(const ADriverId: String); static;
    class function Max(const Arg: String): String; static;
    class function CheckNull(const Arg: String): String; static;
    class function Coalesce(const Arg: String; CoalesceResult: String): String; static;
  end;

implementation

var
  DriverId: String;

{ TPDLFuncs }

class function TPDLFuncs.CheckNull(const Arg: String): String;
begin
Result:=Arg+' IS NULL';
end;

class function TPDLFuncs.Coalesce(const Arg: String;
  CoalesceResult: String): String;
begin
Result:='COALESCE('+Arg+','+CoalesceResult+')';
end;

class procedure TPDLFuncs.Initialize(const ADriverId: String);
begin
DriverId:=ADriverId;
end;

class function TPDLFuncs.Max(const Arg: String): String;
begin
Result:='MAX('+Arg+')';
end;

initialization

  DriverId:=EmptyStr;

end.
