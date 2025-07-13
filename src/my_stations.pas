unit my_stations;

// Ytuner : Custom stations list files support unit.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  md5, common; // DOM, XMLRead, XMLWrite are removed as XML I/O is moved

type
  TMyStation = record
                 MSID, MSName, MSURL, MSURLResolved, MSLogoURL, MSDescription, MSCountry, MSLanguage, MSTags, MSCodec: string;
                 MSBitrate: integer;
               end;

  TMyStations = array of TMyStation;

  TMyStationsGroup = array of record
                                MSCategory: string;
                                MSStations: TMyStations;
                              end;

  TMSStation = record
                 Category: string;
                 Station: TMyStation;
               end;

function GetMyStationByID(AID: string): TMSStation;
procedure AddCategory(ACategoryName: string);
procedure AddStation(ACategoryName: string; AStation: TMyStation);
procedure RemoveCategory(ACategoryName: string);
procedure RemoveStation(ACategoryName: string; AStationID: string);
procedure UpdateStation(ACategoryName: string; AStationID: string; ANewStation: TMyStation);

var
  MyStationsEnabled: boolean = True;
  MyStationsAutoRefreshPeriod: integer = 0;
  MyStationsFileAge: LongInt = 0;
  MyStationsFileCRC32: LongWord = 0;
  MyStations: TMyStationsGroup;

implementation

function GetMyStationByID(AID: string): TMSStation;
var
  i, j: integer;
begin
  Result.Category := '';
  Result.Station.MSID := '';
  for i := 0 to Length(MyStations) - 1 do
  begin
    for j := 0 to Length(MyStations[i].MSStations) - 1 do
    begin
      if MyStations[i].MSStations[j].MSID = AID then
      begin
        Result.Category := MyStations[i].MSCategory;
        Result.Station := MyStations[i].MSStations[j];
        Exit;
      end;
    end;
  end;
end;

procedure AddCategory(ACategoryName: string);
var
  i: integer;
  Found: boolean;
begin
  Found := False;
  for i := 0 to Length(MyStations) - 1 do
  begin
    if MyStations[i].MSCategory = ACategoryName then
    begin
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    SetLength(MyStations, Length(MyStations) + 1);
    MyStations[High(MyStations)].MSCategory := ACategoryName;
    SetLength(MyStations[High(MyStations)].MSStations, 0);
  end;
end;

procedure AddStation(ACategoryName: string; AStation: TMyStation);
var
  i: integer;
  CategoryIndex: integer;
  Found: boolean;
begin
  CategoryIndex := -1;
  for i := 0 to Length(MyStations) - 1 do
  begin
    if MyStations[i].MSCategory = ACategoryName then
    begin
      CategoryIndex := i;
      Break;
    end;
  end;

  if CategoryIndex = -1 then
  begin
    AddCategory(ACategoryName);
    CategoryIndex := High(MyStations);
  end;

  Found := False;
  for i := 0 to Length(MyStations[CategoryIndex].MSStations) - 1 do
  begin
    if MyStations[CategoryIndex].MSStations[i].MSID = AStation.MSID then
    begin
      MyStations[CategoryIndex].MSStations[i] := AStation; // Update existing station
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    SetLength(MyStations[CategoryIndex].MSStations, Length(MyStations[CategoryIndex].MSStations) + 1);
    MyStations[CategoryIndex].MSStations[High(MyStations[CategoryIndex].MSStations)] := AStation;
  end;
end;

procedure RemoveCategory(ACategoryName: string);
var
  i, j: integer;
begin
  for i := 0 to Length(MyStations) - 1 do
  begin
    if MyStations[i].MSCategory = ACategoryName then
    begin
      for j := i to High(MyStations) - 1 do
        MyStations[j] := MyStations[j + 1];
      SetLength(MyStations, Length(MyStations) - 1);
      Break;
    end;
  end;
end;

procedure RemoveStation(ACategoryName: string; AStationID: string);
var
  i, j, CategoryIndex: integer;
begin
  CategoryIndex := -1;
  for i := 0 to Length(MyStations) - 1 do
  begin
    if MyStations[i].MSCategory = ACategoryName then
    begin
      CategoryIndex := i;
      Break;
    end;
  end;

  if CategoryIndex <> -1 then
  begin
    for i := 0 to Length(MyStations[CategoryIndex].MSStations) - 1 do
    begin
      if MyStations[CategoryIndex].MSStations[i].MSID = AStationID then
      begin
        for j := i to High(MyStations[CategoryIndex].MSStations) - 1 do
          MyStations[CategoryIndex].MSStations[j] := MyStations[CategoryIndex].MSStations[j + 1];
        SetLength(MyStations[CategoryIndex].MSStations, Length(MyStations[CategoryIndex].MSStations) - 1);
        Break;
      end;
    end;
  end;
end;

procedure UpdateStation(ACategoryName: string; AStationID: string; ANewStation: TMyStation);
var
  i, CategoryIndex: integer;
begin
  CategoryIndex := -1;
  for i := 0 to Length(MyStations) - 1 do
  begin
    if MyStations[i].MSCategory = ACategoryName then
    begin
      CategoryIndex := i;
      Break;
    end;
  end;

  if CategoryIndex <> -1 then
  begin
    for i := 0 to Length(MyStations[CategoryIndex].MSStations) - 1 do
    begin
      if MyStations[CategoryIndex].MSStations[i].MSID = AStationID then
      begin
        MyStations[CategoryIndex].MSStations[i] := ANewStation;
        Break;
      end;
    end;
  end;
end;

end.