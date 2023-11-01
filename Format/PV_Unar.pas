unit PV_Unar;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-11-01
//Haruhiko Okumura .AR (AR002)

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnar = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnar.Create(Str: TStream);
type TEntry = packed record
       HeadSize: Byte; //=25 + Length(Filename)
       HeadSum: Byte; //algebraic sum mod 256
       Method: array[0..4] of Char; //-lh0-, -lh4-, -lh5-
       PackedSize: Cardinal;
       UnpackedSize: Cardinal;
       Unused: Cardinal;
       B1: Byte; //=$20
       B2: Byte; //=$21
       FNameLength: Byte;
       //FName
    end;
    TEntry2 = packed record
       CRC16: Word;
       B3: Byte; //=$20
       FirstExtHeader: Word; //0 if none
       //first extended header here
    end;


var Entry: TEntry;
    Entry2: TEntry2;
    i: Integer;
    FName: String;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    while FStream.Position < FStream.Size do begin

      FStream.Read(Entry, SizeOf(Entry));

      SetLength(FName, Entry.FNameLength);
      FStream.Read(FName[1], Entry.FNameLength);

      FStream.Read(Entry2, SizeOf(Entry2));

      if Entry2.FirstExtHeader > 0 then FStream.Position := FStream.Position + Entry2.FirstExtHeader;

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;

      if Entry.Method      = '-lh0-' then AFile.PackMethod := pmStore
      else if Entry.Method = '-lh4-' then AFile.PackMethod := pmLH4
      else if Entry.Method = '-lh5-' then AFile.PackMethod := pmLH5
      else                                AFile.PackMethod := pmOther;

      AFile.ModDate := 0;
      AFile.CRC32 := Entry2.CRC16;

      AddFile(AFile);

      FStream.Position := FStream.Position + Entry.PackedSize;
    end;

  except
  end;
end;

end.

