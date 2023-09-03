{*******************************************************}
{                                                       }
{     Модуль вычисления CRC                             }
{     Copyright (c) 2001-2014 by Alex A. Lagodny        }
{                                                       }
{*******************************************************}

unit LA.Utils.CRC;

interface

uses
  System.SysUtils;

type
  TCRCRec = record
    Bit: byte;
    Poly: UInt32;
    Init, XorOut: UInt32;
    ReflIn,ReflOut: boolean;
  end;

  TCRC = class
  public
    class function CRC32(aBuffer: TBytes; aFrom, aTo: Integer): UInt32;
    class function CRC16(aBuffer: TBytes): TBytes;
    class function CRC8(aBuffer: TBytes): Byte;
    class function CRC8Ext(const aBuffer; aIndex, aCount: Integer): Byte;

    class function Revers(w: UInt32; j: integer): UInt32;
    class function CRCRec(bit:byte; Poly:UInt32; Init,XorOut:UInt32; ReflIn,ReflOut: boolean): TCRCRec;
    class procedure CreatTableCRC(T:TCRCRec; Tabel:Pointer);

    class function GetCRC(t: TCRCRec; p: pointer; length: integer; Tabel: Pointer): UInt32;

    class function Test: UInt32;
    class function Test16PA001(arr: TBytes; poly: Cardinal): UInt32;
  end;



implementation

const
  arrayCRC32: Array[0..255] of UInt32 =
     ($00000000, $77073096, $EE0E612C, $990951BA,
      $076DC419, $706AF48F, $E963A535, $9E6495A3,
      $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
      $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
      $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
      $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
      $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
      $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
      $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
      $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
      $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
      $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
      $26D930AC, $51DE003A, $C8D75180, $BFD06116,
      $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
      $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
      $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
      $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
      $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
      $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
      $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
      $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
      $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
      $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
      $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
      $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
      $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
      $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
      $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
      $5005713C, $270241AA, $BE0B1010, $C90C2086,
      $5768B525, $206F85B3, $B966D409, $CE61E49F,
      $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
      $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
      $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
      $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
      $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
      $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
      $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
      $F762575D, $806567CB, $196C3671, $6E6B06E7,
      $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
      $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
      $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
      $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
      $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
      $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
      $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
      $CC0C7795, $BB0B4703, $220216B9, $5505262F,
      $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
      $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
      $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
      $9C0906A9, $EB0E363F, $72076785, $05005713,
      $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
      $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
      $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
      $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
      $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
      $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
      $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
      $A7672661, $D06016F7, $4969474D, $3E6E77DB,
      $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
      $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
      $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
      $BAD03605, $CDD70693, $54DE5729, $23D967BF,
      $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
      $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

  arrayCRC16Hi: array[0..255] of byte = (
    $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00,
    $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1,
    $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81,
    $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $01,
    $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0,
    $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80,
    $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $00,
    $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0,
    $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80,
    $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41,
    $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $01,
    $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1,
    $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81,
    $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
    $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01,
    $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1,
    $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80,
    $41, $01, $C0, $80, $41, $00, $C1, $81, $40);

  arrayCRC16Lo: array[0..255] of byte = (
    $00, $C0, $C1, $01, $C3, $03, $02, $C2, $C6, $06, $07, $C7, $05,
    $C5, $C4, $04, $CC, $0C, $0D, $CD, $0F, $CF, $CE, $0E, $0A, $CA,
    $CB, $0B, $C9, $09, $08, $C8, $D8, $18, $19, $D9, $1B, $DB, $DA,
    $1A, $1E, $DE, $DF, $1F, $DD, $1D, $1C, $DC, $14, $D4, $D5, $15,
    $D7, $17, $16, $D6, $D2, $12, $13, $D3, $11, $D1, $D0, $10, $F0,
    $30, $31, $F1, $33, $F3, $F2, $32, $36, $F6, $F7, $37, $F5, $35,
    $34, $F4, $3C, $FC, $FD, $3D, $FF, $3F, $3E, $FE, $FA, $3A, $3B,
    $FB, $39, $F9, $F8, $38, $28, $E8, $E9, $29, $EB, $2B, $2A, $EA,
    $EE, $2E, $2F, $EF, $2D, $ED, $EC, $2C, $E4, $24, $25, $E5, $27,
    $E7, $E6, $26, $22, $E2, $E3, $23, $E1, $21, $20, $E0, $A0, $60,
    $61, $A1, $63, $A3, $A2, $62, $66, $A6, $A7, $67, $A5, $65, $64,
    $A4, $6C, $AC, $AD, $6D, $AF, $6F, $6E, $AE, $AA, $6A, $6B, $AB,
    $69, $A9, $A8, $68, $78, $B8, $B9, $79, $BB, $7B, $7A, $BA, $BE,
    $7E, $7F, $BF, $7D, $BD, $BC, $7C, $B4, $74, $75, $B5, $77, $B7,
    $B6, $76, $72, $B2, $B3, $73, $B1, $71, $70, $B0, $50, $90, $91,
    $51, $93, $53, $52, $92, $96, $56, $57, $97, $55, $95, $94, $54,
    $9C, $5C, $5D, $9D, $5F, $9F, $9E, $5E, $5A, $9A, $9B, $5B, $99,
    $59, $58, $98, $88, $48, $49, $89, $4B, $8B, $8A, $4A, $4E, $8E,
    $8F, $4F, $8D, $4D, $4C, $8C, $44, $84, $85, $45, $87, $47, $46,
    $86, $82, $42, $43, $83, $41, $81, $80, $40);

  arrayCRC8: array[0..255] of byte = (
    0, 94, 188, 226, 97, 63, 221, 131, 194, 156, 126, 32, 163, 253, 31, 65, 157, 195, 33,
    127, 252, 162, 64, 30, 95, 1, 227, 189, 62, 96, 130, 220, 35, 125, 159, 193, 66, 28,
    254, 160, 225, 191, 93, 3, 128, 222, 60, 98, 190, 224, 2, 92, 223, 129, 99, 61, 124,
    34, 192, 158, 29, 67, 161, 255, 70, 24, 250, 164, 39, 121, 155, 197, 132, 218, 56,
    102, 229, 187, 89, 7, 219, 133, 103, 57, 186, 228, 6, 88, 25, 71, 165, 251, 120, 38,
    196, 154, 101, 59, 217, 135, 4, 90, 184, 230, 167, 249, 27, 69, 198, 152, 122, 36,
    248, 166, 68, 26, 153, 199, 37, 123, 58, 100, 134, 216, 91, 5, 231, 185, 140, 210,
    48, 110, 237, 179, 81, 15, 78, 16, 242, 172, 47, 113, 147, 205, 17, 79, 173, 243,
    112, 46, 204, 146, 211, 141, 111, 49, 178, 236, 14, 80, 175, 241, 19, 77, 206, 144,
    114, 44, 109, 51, 209, 143, 12, 82, 176, 238, 50, 108, 142, 208, 83, 13, 239, 177,
    240, 174, 76, 18, 145, 207, 45, 115, 202, 148, 118, 40, 171, 245, 23, 73, 8, 86, 180,
    234, 105, 55, 213, 139, 87, 9, 235, 181, 54, 104, 138, 212, 149, 203, 41, 119, 244,
    170, 72, 22, 233, 183, 85, 11, 136, 214, 52, 106, 43, 117, 151, 201, 74, 20, 246,
    168, 116, 42, 200, 150, 21, 75, 169, 247, 182, 232, 10, 84, 215, 137, 107, 53);

//  arrayCRC8: array[0..255] of byte = (
//    $00, $31, $62, $53, $C4, $F5, $A6, $97,
//    $B9, $88, $DB, $EA, $7D, $4C, $1F, $2E,
//    $43, $72, $21, $10, $87, $B6, $E5, $D4,
//    $FA, $CB, $98, $A9, $3E, $0F, $5C, $6D,
//    $86, $B7, $E4, $D5, $42, $73, $20, $11,
//    $3F, $0E, $5D, $6C, $FB, $CA, $99, $A8,
//    $C5, $F4, $A7, $96, $01, $30, $63, $52,
//    $7C, $4D, $1E, $2F, $B8, $89, $DA, $EB,
//    $3D, $0C, $5F, $6E, $F9, $C8, $9B, $AA,
//    $84, $B5, $E6, $D7, $40, $71, $22, $13,
//    $7E, $4F, $1C, $2D, $BA, $8B, $D8, $E9,
//    $C7, $F6, $A5, $94, $03, $32, $61, $50,
//    $BB, $8A, $D9, $E8, $7F, $4E, $1D, $2C,
//    $02, $33, $60, $51, $C6, $F7, $A4, $95,
//    $F8, $C9, $9A, $AB, $3C, $0D, $5E, $6F,
//    $41, $70, $23, $12, $85, $B4, $E7, $D6,
//    $7A, $4B, $18, $29, $BE, $8F, $DC, $ED,
//    $C3, $F2, $A1, $90, $07, $36, $65, $54,
//    $39, $08, $5B, $6A, $FD, $CC, $9F, $AE,
//    $80, $B1, $E2, $D3, $44, $75, $26, $17,
//    $FC, $CD, $9E, $AF, $38, $09, $5A, $6B,
//    $45, $74, $27, $16, $81, $B0, $E3, $D2,
//    $BF, $8E, $DD, $EC, $7B, $4A, $19, $28,
//    $06, $37, $64, $55, $C2, $F3, $A0, $91,
//    $47, $76, $25, $14, $83, $B2, $E1, $D0,
//    $FE, $CF, $9C, $AD, $3A, $0B, $58, $69,
//    $04, $35, $66, $57, $C0, $F1, $A2, $93,
//    $BD, $8C, $DF, $EE, $79, $48, $1B, $2A,
//    $C1, $F0, $A3, $92, $05, $34, $67, $56,
//    $78, $49, $1A, $2B, $BC, $8D, $DE, $EF,
//    $82, $B3, $E0, $D1, $46, $77, $24, $15,
//    $3B, $0A, $59, $68, $FF, $CE, $9D, $AC
//    );


class function TCRC.CRC16(aBuffer: TBytes): TBytes;
var
  i: Integer;
  tmp: word;
begin
  SetLength(Result, 2);
  Result[0] := $FF;
  Result[1] := $FF;
  for i := Low(aBuffer) to High(aBuffer) do
  begin
    tmp := Result[0] xor aBuffer[i];
    Result[0] := Result[1] xor arrayCRC16Hi[tmp];
    Result[1] := arrayCRC16Lo[tmp];
  end;
end;

class function TCRC.CRC32(aBuffer: TBytes; aFrom, aTo: Integer): UInt32;
var
  i: Integer;
begin
  Result := $FFFFFFFF;
  for i := aFrom to aTo do
  begin
    Result := (Result shr 8) xor arrayCRC32[aBuffer[i] xor (Result and $000000FF)];
  end;
  Result := not Result;
end;

class function TCRC.CRC8(aBuffer: TBytes): Byte;
var
  i: integer;
  CRC: byte;
begin
  Result := 0;
  for i := Low(aBuffer) to High(aBuffer) do
  begin
    CRC := arrayCRC8[Result xor aBuffer[i]];
    Result := CRC;
  end;
end;

class function TCRC.CRC8Ext(const aBuffer; aIndex, aCount: Integer): Byte;
var
  i: NativeUInt;
  CRC: byte;
begin
  Result := 0;
  for i := aIndex to aIndex + aCount - 1 do
  begin
    CRC := arrayCRC8[Result xor PByte(NativeUInt(@aBuffer) + i)^];
    Result := CRC;
  end;
end;


class function TCRC.CRCRec(bit: byte; Poly, Init, XorOut: UInt32; ReflIn, ReflOut: boolean): TCRCRec;
begin
  Result.Init := Init;
  Result.XorOut := XorOut;
  Result.Poly := Poly;
  Result.bit := bit;
  Result.ReflIn := ReflIn;
  Result.ReflOut := ReflOut;
end;

class procedure TCRC.CreatTableCRC(T: TCRCRec; Tabel: Pointer);
type
  TTable = array[0..0] of UInt32;

  PTable = ^TTable;
var
  i, j: Word;
  crc: UInt32;
begin
{$IFOPT R+}
  {$DEFINE RANGEON}
  {$R-}
{$ELSE}
  {$UNDEF RANGEON}
{$ENDIF}
  if (t.ReflIn) then
  begin
    t.Poly := revers(t.Poly, t.bit);
    for i := 0 to 255 do
    begin
      crc := i;
      for j := 0 to 7 do
        if (crc and 1) <> 0 then
          crc := (crc shr 1) xor t.Poly
        else
          crc := (crc shr 1);
      PTable(Tabel)[i] := crc;
    end;
  end
  else
  begin
    for i := 0 to 255 do
    begin
      crc := i shl (t.bit - 8);
      for j := 0 to 7 do
        if (crc and (1 shl (t.bit - 1))) <> 0 then
          crc := (crc shl 1) xor t.Poly
        else
          crc := (crc shl 1);
      PTable(Tabel)[i] := crc mod (1 shl t.bit);
    end;
  end;
{$IFDEF RANGEON}
  {$R+}
  {$UNDEF RANGEON}
{$ENDIF}
end;

// http://forum.sources.ru/index.php?showtopic=354100
class function TCRC.GetCRC(t: TCRCRec; p: pointer; length: integer; Tabel: Pointer): UInt32;
type
  TTable = array[0..0] of UInt32;

  PTable = ^TTable;
var
  crc: UInt32;
  i: integer;
begin
{$IFOPT R+}
  {$DEFINE RANGEON}
  {$R-}
{$ELSE}
  {$UNDEF RANGEON}
{$ENDIF}
  CRC := t.Init;
  if (t.ReflIn) then
    for i := 0 to length - 1 do
      CRC := (CRC shr 8) xor PTable(Tabel)[byte(CRC) xor Byte(TBytes(p)[i])]
  else
  begin
    for i := 0 to length - 1 do
      CRC := (CRC shl 8) xor PTable(Tabel)[byte(CRC shr (t.bit - 8)) xor Byte(TBytes(p)[i])];
  end;
  CRC := CRC xor t.XorOut;
  if (t.ReflOut xor t.ReflIn) then
    CRC := revers(CRC, t.bit);
  GetCRC := CRC shl (32 - t.bit) shr (32 - t.bit);
{$IFDEF RANGEON}
  {$R+}
  {$UNDEF RANGEON}
{$ENDIF}
end;

class function TCRC.Revers(w: UInt32; j: integer): UInt32;
var
  i: Integer;
  p: UInt32;
begin
  p := 0;
  for i := 1 to j do
  begin
    if w and 1 <> 0 then
      p := (p shl 1) or 1
    else
      p := (p shl 1);
    w := w shr 1;
  end;
  revers := p;
end;

class function TCRC.Test: UInt32;
var
  c: UInt32;
  t: TCRCRec;
  tabelCRC: array [0..$FF] of UInt32;
const
//  test: array[0..8] of char = ('1', '2', '3', '4', '5', '6', '7', '8', '9');
  test2: array[0..3] of byte = ($11, $22, $33, $44);
begin
  // A001
  t := CRCRec(16, $8005, $0, $0, true, true);
  CreatTableCRC(t, @TabelCRC);
  c := GetCRC(t, @Test2, Length(Test2), @TabelCRC);
  Result := c;
end;

class function TCRC.Test16PA001(arr: TBytes; poly: Cardinal): UInt32;
var
  c: UInt32;
  t: TCRCRec;
  tabelCRC: array [0..$FF] of UInt32;
begin
  // реверсирование полинома полином $8005 = A001
  t := CRCRec(16, poly, $0, $0, True, True);
  CreatTableCRC(t, @TabelCRC);
  c := GetCRC(t, @arr[0], Length(arr), @TabelCRC);
  Result := c;
end;

end.
