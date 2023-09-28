unit PV_Unlzh;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-16

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

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
      FileDate: Integer;
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
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

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

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := FName;
      FFiles[FCount].Offset := FStream.Position;
      FFiles[FCount].PackedSize := Head.UnPackedSize;
      FFiles[FCount].UnpackedSize := Head.UnpackedSize;
      //FFiles[FCount].PackMethod := pmStore;

      FFiles[FCount].PackMethod := pmLha;

      FStream.Position := FStream.Position + Head.PackedSize;
      Inc(FCount);

      if FStream.Position+1024 >= FStream.Size then break;

    end;
  except
  end;
end;

end.

