unit PDL.Layer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

type
  TPDLConnectionEvent = procedure(AConnection: TObject) of Object;
  TPDLDataSetEvent = procedure(ADataSet: TObject) of Object;
  TPDLQueryEvent = procedure(AQuery: TObject) of Object;
  TPDLFieldByName = function(ADataSet: TObject; AFieldName: String): TField of Object;
  TPDLParamByName = function(ADataset: TObject; AParamName: String): TParam of Object;
  TPDLDataSetBooleanEvent = function(ADataset: TObject): Boolean of Object;
  TPDLDataSetIntegerEvent = function(ADataSet: TObject): Integer of Object;
  TPDLQueryCommand = procedure(AQuery: TObject; ACommand: String);

  { TPDLLayer }

  TPDLLayer = class
  private
    FConnect: TPDLConnectionEvent;
    FStartTransaction: TPDLConnectionEvent;
    FCommit: TPDLConnectionEvent;
    FRollback: TPDLConnectionEvent;
    FOpen: TPDLDataSetEvent;
    FExecute: TPDLQueryEvent;
    FFieldByName: TPDLFieldByName;
    FParamByName: TPDLParamByName;
    FBof: TPDLDataSetBooleanEvent;
    FEof: TPDLDataSetBooleanEvent;
    FCount: TPDLDataSetIntegerEvent;
  protected
    procedure SetConnection(AValue: TPDLConnectionEvent);
    procedure SetStartTransaction(AValue: TPDLConnectionEvent);
    procedure SetCommit(AValue: TPDLConnectionEvent);
    procedure SetRollback(AValue: TPDLConnectionEvent);
    procedure SetOpen(AValue: TPDLDataSetEvent);
    procedure SetFieldByName(AValue: TPDLFieldByName);
    procedure SetParamByName(AValue: TPDLParamByName);
    procedure SetExecute(AValue: TPDLQueryEvent);
    procedure SetBof(AValue: TPDLDataSetBooleanEvent);
    procedure SetEof(AValue: TPDLDataSetBooleanEvent);
    procedure SetCount(AValue: TPDLDataSetIntegerEvent);
    procedure SetFirst(AValue: TPDLDataSetEvent);
    procedure SetLast(AValue: TPDLDataSetEvent);
    procedure SetNext(AValue: TPDLDataSetEvent);
    procedure SetPrior(AValue: TPDLDataSetEvent);
    procedure SetQueryCommand(AValue: TPDLQueryCommand);
  public
    class function Instance: TPDLLayer;
    procedure Connect(AConnection: TObject);
    procedure StartTransaction(AConnection: TObject);
    procedure Commit(AConnection: TObject);
    procedure Rollback(AConnection: TObject);
    procedure Open(ADataSet: TObject);
    procedure Execute(AQuery: TObject);
    function FieldByName(ADataSet: TObject; AFieldName: String): TField;
    function ParamByName(ADataSet: TObject; AParamName: String): TParam;
    function Bof(ADataSet: TObject): Boolean;
    function Eof(ADataSet: TObject): Boolean;
    function Count(ADataSet: TObject): Integer;
    procedure First(ADataSet: TObject);
    procedure Last(ADataSet: TObject);
    procedure Next(ADataSet: TObject);
    procedure Prior(ADataSet: TObject);
    procedure QueryCommand(AQuery: TObject; ACommand: String);
  end;

implementation

{ TPDLLayer }

procedure TPDLLayer.SetConnection(AValue: TPDLConnectionEvent);
begin

end;

procedure TPDLLayer.SetStartTransaction(AValue: TPDLConnectionEvent);
begin

end;

procedure TPDLLayer.SetCommit(AValue: TPDLConnectionEvent);
begin

end;

procedure TPDLLayer.SetRollback(AValue: TPDLConnectionEvent);
begin

end;

procedure TPDLLayer.SetOpen(AValue: TPDLDataSetEvent);
begin

end;

procedure TPDLLayer.SetFieldByName(AValue: TPDLFieldByName);
begin

end;

procedure TPDLLayer.SetParamByName(AValue: TPDLParamByName);
begin

end;

procedure TPDLLayer.SetExecute(AValue: TPDLQueryEvent);
begin

end;

procedure TPDLLayer.SetBof(AValue: TPDLDataSetBooleanEvent);
begin

end;

procedure TPDLLayer.SetEof(AValue: TPDLDataSetBooleanEvent);
begin

end;

procedure TPDLLayer.SetCount(AValue: TPDLDataSetIntegerEvent);
begin

end;

procedure TPDLLayer.SetFirst(AValue: TPDLDataSetEvent);
begin

end;

procedure TPDLLayer.SetLast(AValue: TPDLDataSetEvent);
begin

end;

procedure TPDLLayer.SetNext(AValue: TPDLDataSetEvent);
begin

end;

procedure TPDLLayer.SetPrior(AValue: TPDLDataSetEvent);
begin

end;

procedure TPDLLayer.SetQueryCommand(AValue: TPDLQueryCommand);
begin

end;

class function TPDLLayer.Instance: TPDLLayer;
begin

end;

procedure TPDLLayer.Connect(AConnection: TObject);
begin

end;

procedure TPDLLayer.StartTransaction(AConnection: TObject);
begin

end;

procedure TPDLLayer.Commit(AConnection: TObject);
begin

end;

procedure TPDLLayer.Rollback(AConnection: TObject);
begin

end;

procedure TPDLLayer.Open(ADataSet: TObject);
begin

end;

procedure TPDLLayer.Execute(AQuery: TObject);
begin

end;

function TPDLLayer.FieldByName(ADataSet: TObject; AFieldName: String): TField;
begin

end;

function TPDLLayer.ParamByName(ADataSet: TObject; AParamName: String): TParam;
begin

end;

function TPDLLayer.Bof(ADataSet: TObject): Boolean;
begin

end;

function TPDLLayer.Eof(ADataSet: TObject): Boolean;
begin

end;

function TPDLLayer.Count(ADataSet: TObject): Integer;
begin

end;

procedure TPDLLayer.First(ADataSet: TObject);
begin

end;

procedure TPDLLayer.Last(ADataSet: TObject);
begin

end;

procedure TPDLLayer.Next(ADataSet: TObject);
begin

end;

procedure TPDLLayer.Prior(ADataSet: TObject);
begin

end;

procedure TPDLLayer.QueryCommand(AQuery: TObject; ACommand: String);
begin

end;

end.

