unit PV_Unlzh;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-01

interface

uses
  Classes, SysUtils, PV_Unpack, CRC16_ARC, Dialogs;

  { TUntar }

type
  TUnlzh = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnlzh.Create(Str: TStream);
type THead = packed record
      HeadLen: Byte;   // 0=end of file
      HeadCRC: Byte;
      SignBegin: Byte; // '-'
      l: Byte;         // 'l'
      H: Byte;         // 'h'
      Compression: Byte;
      SignEnd: Byte;  // '-'
      PackedSize: Cardinal;
      UnpackedSize: Cardinal;
      FileTime: Word;
      FileDate: Word;
      ExternalAttr: Byte;
      Level: Byte;
    end;

var Head: THead;
    Pos: Integer;
    FName: String;
    FNameLen: Byte;
    CRC16: Word;
    Host: Byte;
    NextHeadSize: Word;
    NextHeadKind: Byte;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;
  FHasherClass := THasherCRC16_ARC;

  try
    while True do begin

      FStream.Read(Head, SizeOf(Head));

      if Head.Level = 0 then begin
        FStream.Read(FNameLen, 1);
        SetLength(FName, FNameLen);
        FStream.Read(FName[1], FNameLen);

        FStream.Read(CRC16, 2);
      end
      else if Head.Level = 1 then begin
        FStream.Read(FNameLen, 1);
        SetLength(FName, FNameLen);
        FStream.Read(FName[1], FNameLen);

        FStream.Read(CRC16, 2);
        FStream.Read(Host, 1);
      end
      else if Head.Level = 2 then begin
        FStream.Read(CRC16, 2);
        FStream.Read(Host, 1);
      end;

      if Head.Level in [1,2] then begin

        while True do begin
          Pos := FStream.Position;

          FStream.Read(NextHeadSize, 2);

          if NextHeadSize = 0 then break;

          FStream.Read(NextHeadKind, 1);

          if NextHeadKind = 1 then begin
            SetLength(FName, NextHeadSize-3);
            FStream.Read(FName[1], NextHeadSize-3);
          end;

          FStream.Position := Pos + NextHeadSize;
        end;
      end;

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Head.PackedSize;
      AFile.UnpackedSize := Head.UnpackedSize;

      if Head.Compression = ord('0')      then AFile.PackMethod := pmStore
      else if Head.Compression = ord('1') then AFile.PackMethod := pmLh1
      else if Head.Compression = ord('4') then AFile.PackMethod := pmLh4
      else if Head.Compression = ord('5') then AFile.PackMethod := pmLh5
      else if Head.Compression = ord('6') then AFile.PackMethod := pmLh6
      else if Head.Compression = ord('7') then AFile.PackMethod := pmLh7
      else if Head.Compression = ord('x') then AFile.PackMethod := pmLhX
      else                                     AFile.PackMethod := pmOther;

      if Head.Compression = ord('x') then
      AFile.ModDate := Unix2DateTime((Head.FileDate shl 16) + Head.FileTime)
      else
        AFile.ModDate := Dos2DateTime(Head.FileTime, Head.FileDate);
      AFile.CRC32 := CRC16;

      AddFile(AFile);

      FStream.Position := FStream.Position + Head.PackedSize;

      if FStream.Position+1024 >= FStream.Size then break;

    end;
  except
  end;
end;

end.

