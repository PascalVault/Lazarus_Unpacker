unit PV_Unbzip2;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//BZip2 .BZ2

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnbzip2 = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnbzip2.Create(Str: TStream);
type THead = packed record
      Magic: array[0..2] of Char;
      BlockSize: Byte;
    end;


var Head: THead;
    i: Integer;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

     if FCount = FSize then begin
       Inc(FSize, 1000);
       SetLength(FFiles, FSize);
     end;

    FFiles[FCount].Name := '';
    FFiles[FCount].Offset := FStream.Position-4;
    FFiles[FCount].PackedSize := FStream.Size;
    FFiles[FCount].UnpackedSize := 0;
    FFiles[FCount].PackMethod := pmBzip2;

    Inc(FCount);

  except
  end;
end;

end.

