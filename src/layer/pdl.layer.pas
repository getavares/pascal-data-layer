unit PDL.Layer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

type
  { TPDLLayerAbstraction }

  TPDLLayerAbstraction = class
  private
    FName: String;
  protected
    function GetName: String; virtual; abstract;
  public
    procedure Connect(AConnection: TObject); virtual; abstract;
    procedure Disconnect(AConnection: TObject); virtual; abstract;
    procedure StartTransaction(AConnection: TObject); virtual; abstract;
    procedure Commit(AConnection: TObject); virtual; abstract;
    procedure Rollback(AConnection: TObject); virtual; abstract;
    procedure Open(ADataSet: TObject); virtual; abstract;
    procedure Execute(AQuery: TObject); virtual; abstract;
    function FieldByName(ADataSet: TObject; AFieldName: String): TField; virtual; abstract;
    function ParamByName(ADataSet: TObject; AParamName: String): TParam; virtual; abstract;
    function Bof(ADataSet: TObject): Boolean; virtual; abstract;
    function Eof(ADataSet: TObject): Boolean; virtual; abstract;
    function Count(ADataSet: TObject): Integer; virtual; abstract;
    procedure First(ADataSet: TObject); virtual; abstract;
    procedure Last(ADataSet: TObject); virtual; abstract;
    procedure Next(ADataSet: TObject); virtual; abstract;
    procedure Prior(ADataSet: TObject); virtual; abstract;
    procedure QueryCommand(AQuery: TObject; ACommand: String); virtual; abstract;
    procedure QueryConnection(AQuery: TObject; AConnection: TObject); virtual; abstract;
    property Name: String read GetName;
  end;

  TPDLLayer = class
  private
    FSpecializations: TStringList;
    function GetSpecializations(AName: String): TPDLLayerAbstraction;
  public
    class function Instance: TPDLLayer;
    procedure RegisterSpecialization(ASpecialization: TPDLLayerAbstraction);
    property Specializations[AName: String]: TPDLLayerAbstraction read GetSpecializations; default;
  end;

implementation

var
  PDLLayer: TPDLLayer;

{ TPDLLayer }

function TPDLLayer.GetSpecializations(AName: String): TPDLLayerAbstraction;
var
  SpecIndex: Integer;
begin
SpecIndex:=FSpecializations.IndexOf(AName);
Result:=TPDLLayerAbstraction(FSpecializations.Objects[SpecIndex]);
end;

class function TPDLLayer.Instance: TPDLLayer;
begin
if (not Assigned(PDLLayer)) then
  PDLLayer:=TPDLLayer.Create;
Result:=PDLLayer;
end;

procedure TPDLLayer.RegisterSpecialization(ASpecialization: TPDLLayerAbstraction);
begin
if FSpecializations.IndexOf(ASpecialization.Name)=-1 then
  FSpecializations.AddObject(ASpecialization.Name, ASpecialization);
end;

end.

