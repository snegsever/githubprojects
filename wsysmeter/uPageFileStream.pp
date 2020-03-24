{$mode objfpc}{$H+}

unit uPageFileStream;

{ TPageFileStream is a stream in the system page file }

interface

uses
  Classes;

type
  { TPage FileStream does not throw an exception, but sets flags in the state
    If pfsValid is not contained in the States, then the error code explaining
    the cause can be obtained through GetLastError
    Real size (and stream size) may be other, then wanted, because system
    allocate by page or open existing with other settings
  }

  TPageFileStreamStates = set of (
    pfsValid, // The result of calling Create was successful
    pfsOpenExisting // An existing one is open, but no new one is created,
      // but in write mode
    );

  {
    Usage:

    call Create with parameters
    MapSize - must be greater than zero
    MapName - for interprocess communication
    The size of the stream does not change after Create, but it can differ
    from MapSize.

    call CreateForRead if you only need to read
    If used read only mode then don't write to data

    Use the Size property to determine the actual size
  }

  TPageFileStream = class(TCustomMemoryStream)
  strict private
    FMapHandle: THandle;
    FStates: TPageFileStreamStates;
    procedure DoMap(DesiredAccess: DWORD);
  public
    constructor Create(MapSize: SizeUInt; const MapName: UnicodeString = '');
    constructor CreateForRead(const MapName: UnicodeString);
    destructor Destroy; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    property States: TPageFileStreamStates read FStates;
  end;

implementation

uses Windows;

constructor TPageFileStream.Create(MapSize: SizeUInt; const MapName: UnicodeString);
begin
  inherited Create;
  // ! Not PWideChar(MapName), because PWideChar('') <> nil
  FMapHandle := CreateFileMappingW(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
    Hi(MapSize), Lo(MapSize), Pointer(MapName));
  if FMapHandle <> 0 then
  begin
    if GetLastError = ERROR_ALREADY_EXISTS then
      Include(FStates, pfsOpenExisting);
    DoMap(FILE_MAP_WRITE);
  end;
end;

constructor TPageFileStream.CreateForRead(const MapName: UnicodeString);
begin
  inherited Create;
  FMapHandle := OpenFileMappingW(FILE_MAP_READ, False, Pointer(MapName));
  if FMapHandle <> 0 then
  begin
    Include(FStates, pfsOpenExisting);
    DoMap(FILE_MAP_READ);
  end;
end;

destructor TPageFileStream.Destroy;
begin
  if Memory <> nil then
  begin
    UnmapViewOfFile(Memory);
    SetPointer(nil, 0);
  end;
  if FMapHandle <> 0 then
  begin
    CloseHandle(FMapHandle);
    FMapHandle := 0;
  end;
  inherited;
end;

procedure TPageFileStream.DoMap(DesiredAccess: DWORD);
var
  P: Pointer;
  Info: TMemoryBasicInformation;
begin
  P := MapViewOfFile(FMapHandle, DesiredAccess, 0, 0, 0);
  if Assigned(P) then
  begin
    if VirtualQuery(P, @Info, SizeOf(Info)) <> 0 then
    begin
      SetPointer(P, Info.RegionSize);
      Include(FStates, pfsValid);
    end;
  end
  else
    GetLastError;
end;

function TPageFileStream.Write(const Buffer; Count: LongInt): LongInt;
var
  OldPos, NewPos: Int64;
begin
  Result := 0;
  OldPos := Position;
  if (OldPos >= 0) and (Count >= 0) then
  begin
    NewPos := OldPos + Count;
    if (NewPos > 0) and (NewPos < Size) then
    begin
      System.Move(Buffer, PByte(Memory)[OldPos], Count);
      Position := NewPos;
      Result := Count;
    end;
  end;
end;

end.

