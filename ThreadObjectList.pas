unit ThreadObjectList;

interface

uses SysUtils, Classes, Contnrs, Windows, IniFiles, SyncObjs;

type

  { TThreadObjectList class }

  TThreadObjectList = class
  private
    FList: TObjectList;
    FLock: TRTLCriticalSection;
    FDuplicates: TDuplicates;
    function GetItems(Index: Integer): TObject;
    procedure SetItems(Index: Integer; const Value: TObject);
    function getOwnsObjects: Boolean;
    procedure setOwnsObjects(const Value: Boolean);
  protected
    function GetCount: Integer; virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;

  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TObject):Integer; virtual;
    function Insert(APos: Integer; AItem: TObject):Integer; virtual;
    procedure Clear; virtual;
    function LockList: TList;
    procedure Lock;
    procedure Unlock;
    function TryLockList: Boolean;
    procedure Remove(Item: TObject); virtual;
    procedure Delete(Aindex: Integer); virtual;
    procedure Sort(Compare: TListSortCompare);
    procedure UnlockList;
    function IndexOf(AItem: TObject): Integer;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;

    property Count: Integer read GetCount;
    property List: TObjectList read FList;

    property Items[Index: Integer]: TObject read GetItems write SetItems;
    property OwnsObjects: Boolean read getOwnsObjects write setOwnsObjects;
  end;

  TNamedThreadObjectList = class(TThreadObjectList)
  private
    fHashTable: THashedStringList;

    function GetItems(AKey: String): TObject;
    procedure SetItems(AKey: String; const Value: TObject);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(AKey: String; AItem: TObject):Integer;
    procedure Delete(AKey: String);
    function IndexOf(AKey: String): Integer;
    function Get(AKey: string; var AResult: TObject): Boolean;
    property Items[AKey: String]: TObject read GetItems write SetItems; default;
  end;

implementation


{ TThreadObjectList }

constructor TThreadObjectList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FList := TObjectList.Create;
  FDuplicates := dupIgnore;
end;

procedure TThreadObjectList.Delete(Aindex: Integer);
var
	Temp: TObject;
begin
	LockList;
  try
    if (Aindex >= 0) or (Aindex < FList.Count) then
    begin
      Temp := fList[Aindex];
      fList.Delete(AIndex);
      if Temp <> nil then
        Notify(Temp, lnDeleted);
    end;
  finally
    UnlockList;
  end;
end;

destructor TThreadObjectList.Destroy;
begin
  LockList; // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;
end;

function TThreadObjectList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TThreadObjectList.GetItems(Index: Integer): TObject;
begin
	Result := FList.Items[Index];
end;

function TThreadObjectList.getOwnsObjects: Boolean;
begin
  Result := fList.OwnsObjects;
end;

function TThreadObjectList.IndexOf(AItem: TObject): Integer;
begin
  Result := fList.IndexOf(Aitem);
end;

function TThreadObjectList.Insert(APos: Integer; AItem: TObject): Integer;
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
      (FList.IndexOf(AItem) = -1) then
    begin
      FList.Insert(APos, AItem);
      Result := APos;
    end
    else if Duplicates = dupError then begin
      //FList.Error(@SDuplicateItem, Integer(Item));

    end;
  finally
    UnlockList;
  end;
  if AItem <> nil then
    Notify(AItem, lnAdded);  
end;

function TThreadObjectList.Add(Item: TObject): Integer;
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
      (FList.IndexOf(Item) = -1) then
      Result := FList.Add(Item)
    else if Duplicates = dupError then begin
      //FList.Error(@SDuplicateItem, Integer(Item));

    end;
  finally
    UnlockList;
  end;
  if Item <> nil then
    Notify(Item, lnAdded);
end;

procedure TThreadObjectList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

procedure TThreadObjectList.Lock;
begin
  LockList;
end;

function TThreadObjectList.LockList: TList;
begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

procedure TThreadObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;

end;

procedure TThreadObjectList.Remove(Item: TObject);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TThreadObjectList.SetItems(Index: Integer; const Value: TObject);
begin
	FList.Items[Index] := Value;
end;

procedure TThreadObjectList.setOwnsObjects(const Value: Boolean);
begin
  fList.OwnsObjects := Value;
end;

procedure TThreadObjectList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

function TThreadObjectList.TryLockList: Boolean;
begin
  Result := TryEnterCriticalSection(FLock);
end;

procedure TThreadObjectList.Unlock;
begin
  UnlockList;
end;

procedure TThreadObjectList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

{ TNamedThreadObjectList }

function TNamedThreadObjectList.Add(AKey: String; AItem: TObject): Integer;
begin
  SetItems(AKey, AItem);
end;

procedure TNamedThreadObjectList.Clear;
begin
  fHashTable.Clear;
  inherited Clear;
end;

constructor TNamedThreadObjectList.Create;
begin
  inherited Create;
  fHashTable := THashedStringList.Create;
end;

procedure TNamedThreadObjectList.Delete(AKey: String);
var
  i: Integer;
begin
  i := IndexOf(AKey);
  inherited Delete(i);
  fHashTable.Delete(i);
end;

destructor TNamedThreadObjectList.Destroy;
begin
  fHashTable.Free;
  inherited;
end;

function TNamedThreadObjectList.Get(AKey: string;
  var AResult: TObject): Boolean;
var
  i: Integer;
begin
  i := IndexOf(AKey);
  Result := i >= 0;
  if Result then AResult := inherited Items[i];
end;

function TNamedThreadObjectList.GetItems(AKey: String): TObject;
begin
  if not Get(AKey, Result) then
    Result := nil;
end;

function TNamedThreadObjectList.IndexOf(AKey: String): Integer;
begin
  Result := fHashTable.IndexOf(AKey);
end;

procedure TNamedThreadObjectList.SetItems(AKey: String; const Value: TObject);
var
  i: Integer;
begin
  i := IndexOf(AKey);
  if i < 0 then
  begin
    i := inherited Add(Value);
    fHashTable.Add(AKey);
  end else
  begin
    inherited Items[i] := Value;
  end;

end;

end.
