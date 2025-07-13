unit stationsmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpserver, httpdefs, my_stations, common, fphttpclient, fpjson, jsonparser, jsonscanner;

type

  { TStationsManager }

  TStationsManager = class
  private
    procedure HandleRequest(Request: TRequest; Response: TResponse);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterHandlers(AServer: THTTPServer);
  end;

var
  StationsManager: TStationsManager;

implementation

{ TStationsManager }

procedure TStationsManager.HandleRequest(Request: TRequest; Response: TResponse);
var
  WebAppPath: string;
  LMyStations: TMyStations;
  i: integer;
  json: TStringList;
  ACategoryName, AStationID: string;
  AStation: TMyStation;
begin
  WebAppPath := ExtractFilePath(ParamStr(0)) + 'web' + PathDelim;

  if Request.Path = '/manage' then
  begin
    Response.Content := TFileStream.Create(WebAppPath + 'manage.html', fmOpenRead);
    Response.Code := 200;
    Response.ContentType := 'text/html';
  end
  else if Request.Path = '/api/categories' then
  begin
    if Request.Method = 'GET' then
    begin
      LMyStations := TMyStations.Create;
      try
        ReadMyStationsXMLFile(TFileStream.Create(ConfigPath + DirectorySeparator + 'stations.xml', fmOpenRead));
        json := TStringList.Create;
        try
          json.Add('[');
          for i := 0 to LMyStations.Count - 1 do
          begin
            json.Add('  {');
            json.Add('    "name": "' + LMyStations[i].MSCategory + '"');
            json.Add('  }');
            if i < LMyStations.Count - 1 then
              json.Add(',');
          end;
          json.Add(']');
          Response.ContentString := json.Text;
          Response.Code := 200;
          Response.ContentType := 'application/json';
        finally
          json.Free;
        end;
      finally
        LMyStations.Free;
      end;
    end
    else if Request.Method = 'POST' then
    begin
      ACategoryName := Request.ContentFields.Values['name'];
      AddCategory(ACategoryName);
      WriteMyStationsXMLFile(TFileStream.Create(ConfigPath + DirectorySeparator + 'stations.xml', fmOpenWrite));
      Response.Code := 201;
      Response.ContentString := 'Category created';
    end;
  end
  else if Request.Path = '/api/categories/delete' then
  begin
    if Request.Method = 'POST' then
    begin
      ACategoryName := Request.ContentFields.Values['name'];
      RemoveCategory(ACategoryName);
      WriteMyStationsXMLFile(TFileStream.Create(ConfigPath + DirectorySeparator + 'stations.xml', fmOpenWrite));
      Response.Code := 200;
      Response.ContentString := 'Category deleted';
    end;
  end
  else if Request.Path = '/api/stations' then
  begin
    if Request.Method = 'GET' then
    begin
      ACategoryName := Request.QueryFields.Values['category'];
      LMyStations := TMyStations.Create;
      try
        ReadMyStationsXMLFile(TFileStream.Create(ConfigPath + DirectorySeparator + 'stations.xml', fmOpenRead));
        json := TStringList.Create;
        try
          json.Add('[');
          for i := 0 to LMyStations.Count - 1 do
          begin
            if LMyStations[i].MSCategory = ACategoryName then
            begin
              for j := 0 to LMyStations[i].MSStations.Count - 1 do
              begin
                json.Add('  {');
                json.Add('    "id": "' + LMyStations[i].MSStations[j].MSID + '",');
                json.Add('    "name": "' + LMyStations[i].MSStations[j].MSName + '",');
                json.Add('    "url": "' + LMyStations[i].MSStations[j].MSURL + '",');
                json.Add('    "logo": "' + LMyStations[i].MSStations[j].MSLogoURL + '"');
                json.Add('  }');
                if j < LMyStations[i].MSStations.Count - 1 then
                  json.Add(',');
              end;
              break;
            end;
          end;
          json.Add(']');
          Response.ContentString := json.Text;
          Response.Code := 200;
          Response.ContentType := 'application/json';
        finally
          json.Free;
        end;
      finally
        LMyStations.Free;
      end;
    end
    else if Request.Method = 'POST' then
    begin
      ACategoryName := Request.ContentFields.Values['category'];
      AStation.MSName := Request.ContentFields.Values['name'];
      AStation.MSURL := Request.ContentFields.Values['url'];
      AStation.MSLogoURL := Request.ContentFields.Values['logo'];
      AStation.MSID := MD5Print(MD5String(AStation.MSName + AStation.MSURL)).ToUpper;
      AddStation(ACategoryName, AStation);
      WriteMyStationsXMLFile(TFileStream.Create(ConfigPath + DirectorySeparator + 'stations.xml', fmOpenWrite));
      Response.Code := 201;
      Response.ContentString := 'Station created';
    end;
  end
  else if Request.Path = '/api/stations/update' then
  begin
    if Request.Method = 'POST' then
    begin
      ACategoryName := Request.ContentFields.Values['category'];
      AStationID := Request.ContentFields.Values['id'];
      AStation.MSName := Request.ContentFields.Values['name'];
      AStation.MSURL := Request.ContentFields.Values['url'];
      AStation.MSLogoURL := Request.ContentFields.Values['logo'];
      AStation.MSID := AStationID;
      UpdateStation(ACategoryName, AStationID, AStation);
      WriteMyStationsXMLFile(TFileStream.Create(ConfigPath + DirectorySeparator + 'stations.xml', fmOpenWrite));
      Response.Code := 200;
      Response.ContentString := 'Station updated';
    end;
  end
  else if Request.Path = '/api/stations/delete' then
  begin
    if Request.Method = 'POST' then
    begin
      ACategoryName := Request.ContentFields.Values['category'];
      AStationID := Request.ContentFields.Values['id'];
      RemoveStation(ACategoryName, AStationID);
      WriteMyStationsXMLFile(TFileStream.Create(ConfigPath + DirectorySeparator + 'stations.xml', fmOpenWrite));
      Response.Code := 200;
      Response.ContentString := 'Station deleted';
    end;
  end
  else if Request.Path = '/api/import/xml' then
  begin
    if Request.Method = 'POST' then
    begin
      try
        ReadMyStationsXMLFile(Request.ContentStream);
        WriteMyStationsXMLFile(TFileStream.Create(ConfigPath + DirectorySeparator + 'stations.xml', fmOpenWrite));
        Response.Code := 200;
        Response.ContentString := 'XML imported successfully';
      except
        on E: Exception do
        begin
          Response.Code := 500;
          Response.ContentString := 'Error importing XML: ' + E.Message;
        end;
      end;
    end;
  end
  else if Request.Path = '/api/export/xml' then
  begin
    Response.ContentType := 'application/xml';
    Response.SetCustomHeader('Content-Disposition', 'attachment; filename="stations.xml"');
    Response.ContentStream := TMemoryStream.Create;
    try
      WriteMyStationsXMLFile(Response.ContentStream);
      Response.Code := 200;
    except
      on E: Exception do
      begin
        Response.Code := 500;
        Response.ContentString := 'Error exporting XML: ' + E.Message;
      end;
    end;
  end
  
  else if Request.Path = '/api/search/radiobrowser' then
  begin
    if Request.Method = 'GET' then
    begin
      var SearchTerm: string;
      var HttpClient: TFPHttpClient;
      var JsonParser: TJSONParser;
      var JsonValue: TJSONValue;
      var StationArray: TJSONArray;
      var StationObject: TJSONObject;
      var Station: TMyStation;
      var i: integer;

      SearchTerm := Request.QueryFields.Values['term'];
      HttpClient := TFPHttpClient.Create(nil);
      try
        HttpClient.AddHeader('User-Agent', 'YTuner Station Manager/1.0');
        var ResponseStream: TMemoryStream;
        ResponseStream := TMemoryStream.Create;
        try
          HttpClient.Get('https://de1.api.radio-browser.info/json/stations/search?name=' + SearchTerm + '&limit=100', ResponseStream);
          ResponseStream.Position := 0;
          JsonParser := TJSONParser.Create(ResponseStream.DataString);
          try
            JsonValue := JsonParser.Parse;
            if (JsonValue is TJSONArray) then
            begin
              StationArray := TJSONArray(JsonValue);
              json := TStringList.Create;
              json.Add('[');
              for i := 0 to StationArray.Count - 1 do
              begin
                StationObject := TJSONObject(StationArray.Items[i]);
                json.Add('  {');
                json.Add('    "name": "' + StationObject.Get('name').AsString + '",');
                json.Add('    "url": "' + StationObject.Get('url').AsString + '",');
                json.Add('    "url_resolved": "' + StationObject.Get('url_resolved').AsString + '",');
                json.Add('    "logo": "' + StationObject.Get('favicon').AsString + '",');
                json.Add('    "description": "' + StationObject.Get('homepage').AsString + '",');
                json.Add('    "country": "' + StationObject.Get('country').AsString + '",');
                json.Add('    "language": "' + StationObject.Get('language').AsString + '",');
                json.Add('    "tags": "' + StationObject.Get('tags').AsString + '",');
                json.Add('    "codec": "' + StationObject.Get('codec').AsString + '",');
                json.Add('    "bitrate": ' + StationObject.Get('bitrate').AsString
                );
                json.Add('  }');
                if i < StationArray.Count - 1 then
                  json.Add(',');
              end;
              json.Add(']');
              Response.ContentString := json.Text;
              Response.Code := 200;
              Response.ContentType := 'application/json';
            end;
          finally
            JsonParser.Free;
            JsonValue.Free;
          end;
        finally
          ResponseStream.Free;
        end;
      finally
        HttpClient.Free;
      end;
    end;
  end
  else if Request.Path = '/api/search/tunein' then
  begin
    if Request.Method = 'GET' then
    begin
      var SearchTerm: string;
      var HttpClient: TFPHttpClient;
      var JsonParser: TJSONParser;
      var JsonValue: TJSONValue;
      var BodyObject: TJSONObject;
      var ItemArray: TJSONArray;
      var ItemObject: TJSONObject;
      var Station: TMyStation;
      var i: integer;

      SearchTerm := Request.QueryFields.Values['term'];
      HttpClient := TFPHttpClient.Create(nil);
      try
        HttpClient.AddHeader('User-Agent', 'YTuner Station Manager/1.0');
        var ResponseStream: TMemoryStream;
        ResponseStream := TMemoryStream.Create;
        try
          HttpClient.Get('https://opml.radiotime.com/Search.ashx?query=' + SearchTerm + '&render=json', ResponseStream);
          ResponseStream.Position := 0;
          JsonParser := TJSONParser.Create(ResponseStream.DataString);
          try
            JsonValue := JsonParser.Parse;
            if (JsonValue is TJSONObject) then
            begin
              BodyObject := TJSONObject(JsonValue).Get('body').AsJSONObject;
              if (BodyObject is TJSONArray) then
              begin
                ItemArray := TJSONArray(BodyObject);
                json := TStringList.Create;
                json.Add('[');
                for i := 0 to ItemArray.Count - 1 do
                begin
                  ItemObject := TJSONObject(ItemArray.Items[i]);
                  if (ItemObject.Get('type').AsString = 'audio') and (ItemObject.Get('URL').AsString <> '') and (ItemObject.Get('item').AsString = 'station') then
                  begin
                    json.Add('  {');
                    json.Add('    "name": "' + ItemObject.Get('text').AsString + '",');
                    json.Add('    "url": "' + ItemObject.Get('URL').AsString + '",');
                    json.Add('    "logo": "' + ItemObject.Get('image').AsString + '",');
                    json.Add('    "description": "' + ItemObject.Get('subtext').AsString + '",');
                    json.Add('    "codec": "MP3",'); // Default, will be updated by ICY
                    json.Add('    "bitrate": ' + ItemObject.Get('bitrate').AsString + ',');
                    json.Add('    "language": "''');
                    json.Add('  }');
                    if i < ItemArray.Count - 1 then
                      json.Add(',');
                  end;
                end;
                json.Add(']');
                Response.ContentString := json.Text;
                Response.Code := 200;
                Response.ContentType := 'application/json';
              end;
            end;
          finally
            JsonParser.Free;
            JsonValue.Free;
          end;
        finally
          ResponseStream.Free;
        end;
      finally
        HttpClient.Free;
      end;
    end;
  end
  else
  begin
    Response.Code := 404;
    Response.ContentString := 'Not Found';
  end;
end;

constructor TStationsManager.Create;
begin
  inherited Create;
end;

destructor TStationsManager.Destroy;
begin
  inherited Destroy;
end;

procedure TStationsManager.RegisterHandlers(AServer: THTTPServer);
begin
  AServer.RegisterRequestHandler(httpkAny, '/manage', @HandleRequest);
end;

initialization
  StationsManager := TStationsManager.Create;

finalization
  StationsManager.Free;

end.
