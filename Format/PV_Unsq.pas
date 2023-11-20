unit PV_Unsq;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-11-13
//Richard Greenlaw Squeeze (.SQ, .QQQ)

interface

uses
  Classes, SysUtils, PV_Unpack, CRC16_ARC, Dialogs;

  { TUnPak }

type
  TUnsq = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnsq.Create(Str: TStream);
type TEntry = packed record
       Magic: Word; //$FF76
       Checksum: Word; //low 16 bits of sum of bytes (unpacked)
     end;
     TFoot = packed record
       Magic: Word; //$FF77
       Date: Word;
       Time: Word;
       Checksum: Word; //low 16 of the 3 fields above
     end;

var Entry: TEntry;
    Foot: TFoot;
    FName: String;
    PackSize: Int64;
    Offset: Int64;
    i: Integer;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
      FStream.Read(Entry, SizeOf(Entry));

      if (Entry.Magic <> $FF76) then Exit;

      FName := ReadStrNull(FStream);
      Offset := FStream.Position;

      FStream.Position := FStream.Size - 8;
      FStream.Read(Foot, SizeOf(Foot));

      PackSize := FStream.Size - Offset; 

      if Foot.Magic = $FF77 then begin
        AFile.ModDate := Dos2DateTime(Foot.Time, Foot.Date);
      end
      else begin
        PackSize := PackSize - 8;
        AFile.ModDate := 0;
      end;

      AFile.Name := FName;
      AFile.Offset := Offset;
      AFile.PackedSize := PackSize;
      AFile.UnpackedSize := 0;
      AFile.PackMethod := pmSqueeze;
      AFile.CRC32 := 0;

      AddFile(AFile);

  except
  end;
end;

end.

