unit PV_Unrar;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-16

interface

uses
  Classes, SysUtils, PV_Unpack;

type
  { TUnrar }

  TUnrar = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUnrar }

constructor TUnrar.Create(Str: TStream);
type TVolHead = packed record
       CRC: Word;
       Typee: Byte;
       Flags: Word;
       Size: Word;
     end;

     THead = packed record
       PackedSize: Cardinal;
       UnpackedSize: Cardinal;
       HostOS: Byte;
       CRC32: Cardinal;
       FileTime: Word;
       FileDate: Word;
       Version: Byte;
       Method: Byte;
       FNameLen: Word;
       FileAttr: Cardinal;
     end;

var Head: THead;
    Vol: TVolHead;
    FName: String;
    Pos: Integer;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    while True do begin

      FStream.Read(Vol, SizeOf(Vol));

      if Vol.Typee = $74 then begin
        Pos := FStream.Position;

        FStream.Read(Head, SizeOf(Head));

        SetLength(FName, Head.FNameLen);
        FStream.Read(FName[1], Head.FNameLen);

        FStream.Position := Pos + Vol.Size - 7;

        if FCount = FSize then begin
          Inc(FSize, 1000);
          SetLength(FFiles, FSize);
        end;

        FFiles[FCount].Name := FName;
        FFiles[FCount].Offset := FStream.Position;
        FFiles[FCount].PackedSize := Head.UnPackedSize;
        FFiles[FCount].UnpackedSize := Head.UnpackedSize;

        if Head.Method = 48 then FFiles[FCount].PackMethod := pmStore
        else                     FFiles[FCount].PackMethod := pmOther;

        FStream.Position := FStream.Position + Head.PackedSize;
        Inc(FCount);
      end
      else begin
        FStream.Position := FStream.Position + Vol.Size - 7;
      end;

      if FStream.Position >= FStream.Size then break;
    end;
  except
  end;
end;

end.

