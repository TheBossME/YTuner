unit ResStreamUnZipper;

// YTuner: Resource unzipper simple calss unit.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  zipper;

type
  TResStreamUnZipper = class
  strict private
    FInputStream: TMemoryStream;
    FOutputStream: TMemoryStream;
    FResultStream: TMemoryStream;
    procedure DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoOpenInputStream(Sender: TObject; var AStream: TStream);
  public
    function UnZip(const Input: TResourceStream; var Output: TMemoryStream): boolean;
  end;

implementation

uses common;

{$WARN 5024 OFF}
procedure TResStreamUnZipper.DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  FOutputStream:=TMemoryStream.Create;
  AStream:=FOutputStream;
end;
{$WARN 5024 ON}

{$WARN 5024 OFF}
procedure TResStreamUnZipper.DoDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream.Position:=0;
  FResultStream.CopyFrom(AStream,AStream.Size);
  AStream.Free;
end;
{$WARN 5024 ON}

{$WARN 5024 OFF}
procedure TResStreamUnZipper.DoOpenInputStream(Sender: TObject; var AStream: TStream);
begin
  AStream:=FInputStream;
end;
{$WARN 5024 ON}

function TResStreamUnZipper.UnZip(const Input: TResourceStream; var Output: TMemoryStream): boolean;
begin
  Result:=False;
  FInputStream:=TMemoryStream.Create;
  Input.Position:=0;
  FInputStream.CopyFrom(Input,Input.Size);
  FResultStream:=TMemoryStream.Create;
  try
    with TUnZipper.Create do
      try
        OnCreateStream:=@DoCreateStream;
        OnOpenInputStream:=@DoOpenInputStream;
        OnDoneStream:=@DoDoneStream;
        try
          UnZipAllFiles;
        except
          on E: Exception do
            Logging(ltError, string.Join(' ',[{$I %CURRENTROUTINE%}+':',' ('+E.Message+')']));
        end;
      finally
        Free;
      end;
    FResultStream.Position:=0;
    Output.CopyFrom(FResultStream,FResultStream.Size);
    Result:=True;
  finally
    FResultStream.Free;
  end;
end;

end.


