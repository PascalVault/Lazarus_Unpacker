unit PV_Ungzip;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//Gzip .GZ

interface

uses
  Classes, SysUtils, PV_Unpack, CRC32_ISOHDLC, Dialogs;

  { TUnPak }

type
  TUngzip = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUngzip.Create(Str: TStream);
type THead = packed record
      Magic: Word;
      Compression: Byte;
      Flags: Byte;
      Timestamp: Cardinal;
      CompFlags: Byte;
      Host: Byte;
    end;
    TFoot = packed record
      CRC32: Cardinal;
      UnpackedSize: Cardinal;
    end;

var Head: THead;
    Foot: TFoot;
    i: Integer;
    Total: Integer;
    ExtraLen: Word;
    Temp: String;
    TempLen: Integer;
    Poss: Integer;
    FName: String;
    Comment: String;
    CRC16: Word;
    CRC32: Cardinal;
    UnpackSize: Cardinal;
    PackSize: Int64;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;
  FHasherClass := THasherCRC32_ISOHDLC;

  try
    FStream.Read(Head, SizeOf(Head));

    SetLength(Temp, 500);

    if (Head.Flags and $04) = $04 then begin
      FStream.Read(ExtraLen, 2);
      FStream.Position := FStream.Position + ExtraLen;
    end;

    if (Head.Flags and $08) = $08 then begin
      TempLen := FStream.Read(Temp[1], 500);
      Poss := Pos(#0, Temp);
      FName := Copy(Temp, 1, Poss-1);
      FStream.Position := FStream.Position - TempLen + Poss;
    end;

    if (Head.Flags and $10) = $10 then begin
      TempLen := FStream.Read(Temp[1], 500);
      Poss := Pos(#0, Temp);
      Comment := Copy(Temp, 1, Poss-1);
      FStream.Position := FStream.Position - TempLen + Poss
    end;

    if (Head.Flags and $02) = $02 then begin
      FStream.Read(CRC16, 2);
    end;

    PackSize := FStream.Size - FStream.Position - 8;

    AFile.Name := FName;
    AFile.Offset := FStream.Position;
    AFile.PackedSize := PackSize;

    FStream.Position := FStream.Size - 8;

    FStream.Read(CRC32, 4);
    FStream.Read(UnpackSize, 4);

    AFile.UnpackedSize := UnpackSize;
    AFile.PackMethod := pmDeflate;
    AFile.ModDate:= Unix2DateTime(Head.Timestamp);
    AFile.CRC32 := CRC32;

    AddFile(AFile);

  except
  end;
end;

end.

