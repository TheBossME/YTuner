unit stations_xml_io;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, common, md5, my_stations;

procedure WriteStationsToXmlFile(AFileName: string; AStations: TMyStationsGroup);
function ReadStationsFromXmlFile(AFileName: string): TMyStationsGroup;

procedure ConvertIniToXml(AIniFileName, AXmlFileName: string);
procedure ConvertYamlToXml(AYamlFileName, AXmlFileName: string);

implementation

uses IniFiles, fpYaml;

procedure WriteStationsToXmlFile(AFileName: string; AStations: TMyStationsGroup);
var
  XMLDoc: TXMLDocument;
  Root, ListOfItems, Item: TDOMElement;
  i, j, ItemIndex: integer;
  Now: TDateTime;
  NowStr: string;
  MimeType: string;
begin
  XMLDoc := TXMLDocument.Create;
  try
    Root := XMLDoc.CreateElement('MyStations');
    XMLDoc.AppendChild(Root);

    Root.AppendChild(XMLDoc.CreateElement('Version')).TextContent := '1.1';

    ListOfItems := Root.AppendChild(XMLDoc.CreateElement('ListOfItems'));

    ItemIndex := 0;
    Now := SysUtils.Now;
    NowStr := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

    for i := 0 to Length(AStations) - 1 do
    begin
      for j := 0 to Length(AStations[i].MSStations) - 1 do
      begin
        Item := ListOfItems.AppendChild(XMLDoc.CreateElement('Item'));
        Item.SetAttribute('id', string(AStations[i].MSStations[j].MSID));
        Item.SetAttribute('idx', IntToStr(ItemIndex));

        Item.AppendChild(XMLDoc.CreateElement('Name')).TextContent := AStations[i].MSStations[j].MSName;
        Item.AppendChild(XMLDoc.CreateElement('URL')).TextContent := AStations[i].MSStations[j].MSURL;
        Item.AppendChild(XMLDoc.CreateElement('URLResolved')).TextContent := AStations[i].MSStations[j].MSURLResolved;
        Item.AppendChild(XMLDoc.CreateElement('Description')).TextContent := AStations[i].MSStations[j].MSDescription;
        Item.AppendChild(XMLDoc.CreateElement('Logo')).TextContent := AStations[i].MSStations[j].MSLogoURL;
        Item.AppendChild(XMLDoc.CreateElement('Genres')).TextContent := AStations[i].MSStations[j].MSTags;
        Item.AppendChild(XMLDoc.CreateElement('Languages')).TextContent := AStations[i].MSStations[j].MSLanguage;
        Item.AppendChild(XMLDoc.CreateElement('Location')).TextContent := AStations[i].MSStations[j].MSCountry;

        MimeType := 'audio/' + AStations[i].MSStations[j].MSCodec.ToLower;
        if AStations[i].MSStations[j].MSCodec = '' then
          MimeType := 'audio/mpeg';
        Item.AppendChild(XMLDoc.CreateElement('Mime')).TextContent := MimeType;

        Item.AppendChild(XMLDoc.CreateElement('Codec')).TextContent := AStations[i].MSStations[j].MSCodec;
        if AStations[i].MSStations[j].MSCodec = '' then
          Item.AppendChild(XMLDoc.CreateElement('Codec')).TextContent := 'MP3';

        Item.AppendChild(XMLDoc.CreateElement('Bitrate')).TextContent := IntToStr(AStations[i].MSStations[j].MSBitrate);
        if AStations[i].MSStations[j].MSBitrate = 0 then
          Item.AppendChild(XMLDoc.CreateElement('Bitrate')).TextContent := '128';

        Item.AppendChild(XMLDoc.CreateElement('LastCheckedOK')).TextContent := '1';
        Item.AppendChild(XMLDoc.CreateElement('LastCheckedOKTime')).TextContent := NowStr;
        Item.AppendChild(XMLDoc.CreateElement('LastCheckedTime')).TextContent := NowStr;

        Inc(ItemIndex);
      end;
    end;

    XMLWrite.WriteXMLFile(AStream, XMLDoc);
  finally
    XMLDoc.Free;
  end;
end;

function ReadStationsFromXmlFile(AFileName: string): TMyStationsGroup;
var
  XMLDoc: TXMLDocument;
  Root, ListOfItems, Item: TDOMElement;
  i: integer;
  LMyStation: TMyStation;
  LCategoryName: string;
  ResultStations: TMyStationsGroup;
  Node: TDOMNode;
begin
  Result := nil;
  XMLDoc := TXMLDocument.Create;
  try
    XMLRead.ReadXMLFile(AFileName, XMLDoc);
    Root := XMLDoc.DocumentElement;
    if Assigned(Root) then
    begin
      ListOfItems := nil;
      for i := 0 to Root.ChildNodes.Count - 1 do
      begin
        Node := Root.ChildNodes.Item[i];
        if (Node.NodeType = Node_Element) and (Node.NodeName = 'ListOfItems') then
        begin
          ListOfItems := Node as TDOMElement;
          Break;
        end;
      end;
      if Assigned(ListOfItems) then
      begin
        SetLength(ResultStations, 0); // Initialize empty array
        for i := 0 to ListOfItems.ChildNodes.Count - 1 do
        begin
          Node := ListOfItems.ChildNodes.Item[i];
          if (Node.NodeType = Node_Element) and (Node.NodeName = 'Item') then
          begin
            Item := Node as TDOMElement;
            LMyStation.MSID := Item.GetAttribute('id');
            LMyStation.MSName := Item.GetElementsByTagName('Name').Item[0].TextContent;
            LMyStation.MSURL := Item.GetElementsByTagName('URL').Item[0].TextContent;
            LMyStation.MSURLResolved := Item.GetElementsByTagName('URLResolved').Item[0].TextContent;
            LMyStation.MSDescription := Item.GetElementsByTagName('Description').Item[0].TextContent;
            LMyStation.MSLogoURL := Item.GetElementsByTagName('Logo').Item[0].TextContent;
            LMyStation.MSTags := Item.GetElementsByTagName('Genres').Item[0].TextContent;
            LMyStation.MSLanguage := Item.GetElementsByTagName('Languages').Item[0].TextContent;
            LMyStation.MSCountry := Item.GetElementsByTagName('Location').Item[0].TextContent;
            LMyStation.MSCodec := Item.GetElementsByTagName('Codec').Item[0].TextContent;
            TryStrToInt(Item.GetElementsByTagName('Bitrate').Item[0].TextContent, LMyStation.MSBitrate);

            // Determine category from tags or create a default one
            if LMyStation.MSTags <> '' then
              LCategoryName := LMyStation.MSTags
            else
              LCategoryName := 'Imported';

            // Add to a temporary category list to build TMyStationsGroup
            // This part needs to be carefully integrated with the existing AddCategory/AddStation logic
            // For now, I'll just add to a flat list and convert later if needed.
            SetLength(ResultStations, Length(ResultStations) + 1);
            ResultStations[High(ResultStations)].MSCategory := LCategoryName;
            SetLength(ResultStations[High(ResultStations)].MSStations, 1);
            ResultStations[High(ResultStations)].MSStations[0] := LMyStation;
          end;
        end;
        Result := ResultStations;
      end;
    end;
  finally
    XMLDoc.Free;
  end;
end;

procedure ConvertIniToXml(AIniFileName, AXmlFileName: string);
var
  Ini: TIniFile;
  Doc: TXMLDocument;
  Root, List, Item: TDOMElement;
  Sections: TStringList;
  i: Integer;
  StationName, StationURL: string;
begin
  Ini := TIniFile.Create(AIniFileName);
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);

    Doc := TXMLDocument.Create;
    try
      Root := Doc.CreateElement('MyStations');
      Doc.AppendChild(Root);

      List := Doc.CreateElement('ListOfItems');
      Root.AppendChild(List);

      for i := 0 to Sections.Count - 1 do
      begin
        StationName := Sections[i];
        StationURL := Ini.ReadString(StationName, 'URL', '');

        Item := Doc.CreateElement('Item');
        Item.SetAttribute('id', MD5Print(MD5String(StationName + StationURL)).ToUpper);
        Item.SetAttribute('idx', IntToStr(i));

        Item.AppendChild(Doc.CreateElement('Name')).TextContent := StationName;
        Item.AppendChild(Doc.CreateElement('URL')).TextContent := StationURL;
        Item.AppendChild(Doc.CreateElement('URLResolved')).TextContent := StationURL; // Assuming resolved URL is the same for INI
        Item.AppendChild(Doc.CreateElement('Description')).TextContent := '';
        Item.AppendChild(Doc.CreateElement('Logo')).TextContent := Ini.ReadString(StationName, 'Logo', '');
        Item.AppendChild(Doc.CreateElement('Genres')).TextContent := StationName; // Use section name as genre
        Item.AppendChild(Doc.CreateElement('Languages')).TextContent := '';
        Item.AppendChild(Doc.CreateElement('Location')).TextContent := '';
        Item.AppendChild(Doc.CreateElement('Mime')).TextContent := 'audio/mpeg'; // Default MIME type
        Item.AppendChild(Doc.CreateElement('Codec')).TextContent := 'MP3'; // Default Codec
        Item.AppendChild(Doc.CreateElement('Bitrate')).TextContent := '128'; // Default Bitrate
        Item.AppendChild(Doc.CreateElement('LastCheckedOK')).TextContent := '1';
        Item.AppendChild(Doc.CreateElement('LastCheckedOKTime')).TextContent := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
        Item.AppendChild(Doc.CreateElement('LastCheckedTime')).TextContent := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

        List.AppendChild(Item);
      end;

      XMLWrite.WriteXMLFile(AXmlFileName, Doc);
    finally
      Doc.Free;
    end;
  finally
    Ini.Free;
    Sections.Free;
  end;
end;

procedure ConvertYamlToXml(AYamlFileName, AXmlFileName: string);
var
  Yaml: TYamlDocument;
  RootNode, CategoryNode, StationNode: TYamlNode;
  Doc: TXMLDocument;
  Root, List, Item: TDOMElement;
  i, j: Integer;
  StationName, StationURL, StationLogo: string;
begin
  Yaml := TYamlDocument.Create;
  Doc := TXMLDocument.Create;
  try
    Yaml.LoadFromFile(AYamlFileName);
    RootNode := Yaml.Documents[0].Root;

    Root := Doc.CreateElement('MyStations');
    Doc.AppendChild(Root);

    List := Doc.CreateElement('ListOfItems');
    Root.AppendChild(List);

    for i := 0 to RootNode.Count - 1 do
    begin
      CategoryNode := RootNode.Nodes[i];
      for j := 0 to CategoryNode.Count - 1 do
      begin
        StationNode := CategoryNode.Nodes[j];
        StationName := StationNode.AsScalar;
        StationURL := StationNode.Value.AsString;
        StationLogo := ''; // YAML doesn't typically store logo directly in this structure

        // Extract logo if present in URL (e.g., url|logo)
        if Pos('|', StationURL) > 0 then
        begin
          StationLogo := Copy(StationURL, Pos('|', StationURL) + 1, Length(StationURL));
          StationURL := Copy(StationURL, 1, Pos('|', StationURL) - 1);
        end;

        Item := Doc.CreateElement('Item');
        Item.SetAttribute('id', MD5Print(MD5String(StationName + StationURL)).ToUpper);
        Item.SetAttribute('idx', IntToStr(j));

        Item.AppendChild(Doc.CreateElement('Name')).TextContent := StationName;
        Item.AppendChild(Doc.CreateElement('URL')).TextContent := StationURL;
        Item.AppendChild(Doc.CreateElement('URLResolved')).TextContent := StationURL; // Assuming resolved URL is the same for YAML
        Item.AppendChild(Doc.CreateElement('Description')).TextContent := '';
        Item.AppendChild(Doc.CreateElement('Logo')).TextContent := StationLogo;
        Item.AppendChild(Doc.CreateElement('Genres')).TextContent := CategoryNode.AsScalar; // Use category name as genre
        Item.AppendChild(Doc.CreateElement('Languages')).TextContent := '';
        Item.AppendChild(Doc.CreateElement('Location')).TextContent := '';
        Item.AppendChild(Doc.CreateElement('Mime')).TextContent := 'audio/mpeg'; // Default MIME type
        Item.AppendChild(Doc.CreateElement('Codec')).TextContent := 'MP3'; // Default Codec
        Item.AppendChild(Doc.CreateElement('Bitrate')).TextContent := '128'; // Default Bitrate
        Item.AppendChild(Doc.CreateElement('LastCheckedOK')).TextContent := '1';
        Item.AppendChild(Doc.CreateElement('LastCheckedOKTime')).TextContent := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
        Item.AppendChild(Doc.CreateElement('LastCheckedTime')).TextContent := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);

        List.AppendChild(Item);
      end;
    end;

    XMLWrite.WriteXMLFile(AXmlFileName, Doc);
  finally
    Yaml.Free;
    Doc.Free;
  end;
end;

end.
