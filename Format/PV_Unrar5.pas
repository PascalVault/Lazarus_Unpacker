unit PV_Unrar5;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-16

interface

uses
  Classes, SysUtils, PV_Unpack, CRC32_ISOHDLC, Dialogs;

type
  { TUnrar5 }

  TUnrar5 = class(TUnpack)
  private
    function ReadVint(F: TStream): Int64;
  public
    constructor Create(Str: TStream); override;
  end;


implementation
 
{ TUnrar5 }

function TUnrar5.ReadVint(F: TStream): Int64;
var B: Byte;
    i: Integer;
begin
  Result := 0;

  for i:=0 to 7 do begin
    F.Read(B, 1);
    Result := Result + ((B and $7F) shl (i*7));
    if (B shr 7) and 1 = 0 then break;
  end;
end;

constructor TUnrar5.Create(Str: TStream);
type TMainHead = record
       CRC32: Cardinal;
       Size: Int64;
       Typee: Int64;
       Flags: Int64;
       ExtraSize: Int64;
       ArchiveFlags: Int64;
       VolumeNum: Int64;
     end;

     THead = record
       Flags: Int64;
       ExtraSize: Int64;
       DataSize: Int64;
       FileFlags: Int64;
       UnpackedSize: Int64;
       Attrib: Int64;
       UnixTime: Cardinal;
       DataCRC32: Cardinal;
       Compression: Int64;
       HostOS: Int64;
       FNameLen: Int64;
       FName: String;
     end;

var Head: THead;
    Main: TMainHead;
    FName: String;
    Pos: Integer;
    Magic: Cardinal;
    Magic2: Cardinal;
    Method: Integer;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;
  FHasherClass := THasherCRC32_ISOHDLC;

  FStream.Read(Magic, 4);
  FStream.Read(Magic2, 4);

  try
    while True do begin

      FStream.Read(Main.CRC32, 4);
      Main.Size := ReadVint(FStream);
      Pos := FStream.Position;
      Main.Typee := ReadVint(FStream);

      if Main.Typee = 1 then begin //main header
          Main.Flags := ReadVint(FStream);

          if (Main.Flags and $0001) = $0001 then
            Main.ExtraSize := ReadVint(FStream);

          Main.ArchiveFlags := ReadVint(FStream);

          if (Main.ArchiveFlags and $0002) = $0002 then
            Main.VolumeNum := ReadVint(FStream);

          FStream.Position := Pos + Main.Size;
      end
      else if Main.Typee = 2 then begin //file header
        Head.Flags := ReadVint(FStream);

        if (Head.Flags and $0001) = $0001 then
          Head.ExtraSize := ReadVint(FStream);

        if (Head.Flags and $0002) = $0002 then
          Head.DataSize := ReadVint(FStream);   //for files = PackedSize

        Head.FileFlags := ReadVint(FStream);
        Head.UnpackedSize := ReadVint(FStream);
        Head.Attrib := ReadVint(FStream);

        if (Head.FileFlags and $0002) = $0002 then begin
          FStream.Read(Head.UnixTime, 4);
        end;

        if (Head.FileFlags and $0004) = $0004 then
          FStream.Read(Head.DataCRC32, 4);

        Head.Compression := ReadVint(FStream);
        Head.HostOS := ReadVint(FStream);
        Head.FNameLen := ReadVint(FStream);
        Method := (Head.Compression shr 8) and 7;

        SetLength(FName, Head.FNameLen);
        FStream.Read(FName[1], Head.FNameLen);

        FStream.Position := Pos + Main.size;

        AFile.Name := FName;
        AFile.Offset := FStream.Position;
        AFile.PackedSize := Head.DataSize;
        AFile.UnpackedSize := Head.UnpackedSize;
        AFile.ModDate := 0;
        AFile.CRC32 := Head.DataCRC32;

        if Method = 0 then AFile.PackMethod := pmStore
        else               AFile.PackMethod := pmOther;

        AddFile(AFile);

        FStream.Position := FStream.Position + Head.DataSize;
      end;


      if FStream.Position >= FStream.Size then break;
    end;
  except
  end;
end;

end.

