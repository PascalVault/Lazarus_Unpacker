# Lazarus_Unpacker
PV_Unpacker - simple pure Pascal library to unpack various archives (ZIP, RAR, LZMA, TAR...)

See also:
https://github.com/PascalVault/Lazarus_Packer

## Supported formats (45+) ##
- .ZIP, .JAR, .CBZ, .DOCX (store, deflate, lzma, bzip2)
- .RAR, .CBR (version 4 and 5; store)
- .TAR, .CBT
- .ARJ
- BlackHole .BH
- .BIG
- BGA .BZA/.GZA
- .CPIO
- .DPK
- .DRS
- .FTG
- .GZIP, .GZ
- .BZIP2, .BZ2
- .HA
- .LBR
- .LZH, .LHA (store, lh1, lh4, lh5, lh6, lh7, lhx)
- .ZOO (store, lzh)
- .AR (store, lh4, lh5)
- .LZ, .LZMA
- .PCK
- Homm3 .LOD
- Quake .PAK
- Doom .WAD
- Quake .WAD
and more!

## Unsupported ###
- encrypted archives
- RAR files that use method different than "store"
- ARJ that use method different than "store"

## Usage ###
    use PV_Unpacker;
    ...
    var Unp: TUnpacker;
    begin
      Unp := TUnpacker.Create(OpenDialog1.Filename);
    
      if Unp.GetFormat = '' then ShowMessage('Unsupported');
    
      for i:=0 to Unp.Count-1 do begin
        Memo1.Lines.Add('name ' + Unp.GetName(i) + ', crc ' + IntToHex( Unp.GetCRC(i)) + ', date ' +  DateToStr(Unp.GetDate(i)) + ', packed size ' + IntToStr(Unp.GetPackedSize(i)) );
        if Unp.CanUnpack(i) then Unp.Extract(i, Unp.GetName(i) );        
      end;
      Unp.Free;
    end;  
