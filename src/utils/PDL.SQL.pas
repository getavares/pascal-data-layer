unit PDL.SQL;

interface

uses
  SysUtils, Classes;

type
  IPDLExpression = interface
    procedure Validate;
    function ToString: String;
  end;

  TPDLExpressionList = class(TInterfaceList)
  public
    function Add(AExpression: IPDLExpression): TPDLExpressionList; overload;
    function Remove(AExpression: IPDLExpression): TPDLExpressionList; overload;
  end;

  IPDLLogicOperator = interface
  end;

  TPDLConditionList = class(TInterfaceList)
  public
    function Add(AExpression: IPDLExpression): TPDLConditionList; overload;
    function Add(AOperator: TLogicOperator; AExpression: IPDLExpression): TPDLConditionList; overload;
    function Remove(AExpression: IPDLExpression): TPDLConditionList; overload;
  end;

  IPDLConditionExpression = interface(IPDLExpression)
    function GetConditions: TPDLConditionList;
  end;

  IPDLSelect = interface(IPDLExpression)
    function GetColumns: TPDLExpressionList;
    function GetFrom: IPDLSelect;
    function SetFrom(ASource: IPDLExpression): IPDLSelect;
    function GetJoins: TPDLExpressionList;
    function GetGroupBy: TPDLExpressionList;
    function GetHaving: TPDLExpressionList;
    function GetOrderBy: TPDLExpressionList;
  end;

  IPDLField = interface(IPDLExpression)
  end;

  TPDLFieldList = class(TInterfaceList)
  public
    function Add(AField: IPDLField): TPDLFieldList; overload;
    function Remove(AField: IPDLField): TPDLFieldList; overload;
  end;

  IPDLValue = interface(IPDLExpression)
  end;

  IPDLTable = interface(IPDLExpression)
  end;

  TPDLFieldValueList = class(TInterfaceList)
    function Add(AField: IPDLField, AValue: IPDLExpression): TPDLFieldValueList; overload;
    function Remove(AField: IPDLField): TPDLFieldValueList; overload;
  end;

  IPDLTableConditionExpression = interface(IPDLConditionExpression)
    function GetTable: IPDLTable;
    procedure SetTable(ATable: IPDLTable);
  end;

  IPDLUpdate = interface(IPDLTableConditionExpression)
    function GetColumns: TPDLFieldList;
  end;

  IPDLDelete = interface(IPDLConditionExpression)
  end;

  IPDLInsert = interface(IPDLExpression)
    function GetFields: TPDLFieldValueList
    function GetTable: IPDLTable;
    function SetTable(ATable: IPDLTable);
  end;

  IPDLInsertSelect = interface(IPDLExpression)
    function GetFields: TPDLFieldList;
    function GetTable: IPDLTable;
    procedure SetTable(ATable: IPDLTable);
    procedure SetSelect(ASelect: IPDLSelect);
  end;

  TPDLSQL = class sealed
  public
    class function Instance: TPDLSQL;
  end;

implementation

end.
