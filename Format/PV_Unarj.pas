unit PV_Unarj;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//ARJ .ARJ

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUntar }

type
  TUnarj = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnarj.Create(Str: TStream);
type THead = packed record
        Magic: Word; //=$ea60
        HeadSize: Word;
        HeadSize2: Byte;
        Version: Byte; //4
        MinVersion: Byte;  //1
        Host: Byte; //0= dos, 2=unix, 4=mac
        Flags: Byte; //0=no pass
        Security: Byte;
        Typee: Byte; //0=binary
        Reserved: Byte;
        Date: Word; //msdos
        Time: Word;//msdos
        PackedSize: Cardinal;
        UnpackedSize: Cardinal;
        CRC32: Cardinal;
        Offset: Word;                 //11
        Attributes: Word;             //868
        Hostdata: Word;//not used
     end;

     TEntryHead = packed record
       Magic: Word; //=$ea60
       HeadSize: Word;
     end;

     TEntry = packed record
       HeadSize2: Byte;
       Version: Byte; //4
       MinVersion: Byte;  //1
       Host: Byte; //0= dos, 2=unix, 4=mac
       Flags: Byte; //0=no pass
       Compression: Byte;//0=stored
       Typee: Byte; //0=binary
       Reserved: Byte;
       Time: Word;//msdos
       Date: Word; //msdos
       PackedSize: Cardinal;
       UnpackedSize: Cardinal;
       CRC32: Cardinal;
       Offset: Word;                 //11
       Attributes: Word;             //868
       Hostdata: Word;//not used
//34 bytes all the above


    //file name null terminated
    //comment null terminated
    //crc32 of basic header

    //packed file here

    end;

var Head: THead;
    Entry: TEntry;
    EntryHead: TEntryHead;
    Poss: Integer;
    FName: String;
    TempLen: Integer;
    Temp: String;
    Comment: String;
    HeadCRC32: Cardinal;
    ExtendedSize: Word;
    HeadFlag: Byte;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  SetLength(Temp, 50000);

  try
    FStream.Read(Head, SizeOf(Head));

    TempLen := FStream.Read(Temp[1], 500);
    Poss := Pos(#0, Temp);
    FName := Copy(Temp, 1, Poss-1);
    FStream.Position := FStream.Position - TempLen + Poss;


    TempLen := FStream.Read(Temp[1], 50000);
    Poss := Pos(#0, Temp);
    Comment := Copy(Temp, 1, Poss-1);
    FStream.Position := FStream.Position - TempLen + Poss;

    FStream.Read(ExtendedSize, 2);
    FStream.Read(HeadCRC32, 4);


    while True do begin
      FStream.Read(EntryHead, SizeOf(EntryHead));

      if EntryHead.HeadSize = 0 then break; //end or archive

      FStream.Read(Entry, SizeOf(Entry));

      TempLen := FStream.Read(Temp[1], 500);
      Poss := Pos(#0, Temp);
      FName := Copy(Temp, 1, Poss-1);
      FStream.Position := FStream.Position - TempLen + Poss;

      FStream.Read(ExtendedSize, 2);
      FStream.Read(HeadCRC32, 4);
      FStream.Read(HeadFlag, 1);

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;

      if Entry.Compression = 0 then AFile.PackMethod := pmStore
      else                          AFile.PackMethod := pmOther;
      //AFile.PackMethod := pmStore ;

      AFile.ModDate := Dos2DateTime(Entry.Time, Entry.Date);
      AFile.CRC32 := Entry.CRC32;

      AddFile(AFile);

      FStream.Position := FStream.Position + Entry.PackedSize;

      if FStream.Position > FStream.Size-1 then break;

    end;
  except
  end;
end;

end.

