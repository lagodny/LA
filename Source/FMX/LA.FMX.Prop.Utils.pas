unit LA.FMX.Prop.Utils;

interface

uses
  System.Classes, System.Types,
  System.Generics.Collections,
  System.SysUtils;

type
// https://en.delphipraxis.net/topic/4508-read-and-reapply-design-time-properties-to-specific-controls-runtime/?do=findComment&comment=39256
  TDFMSearch = class
  private
    FFormClasses: TList<string>;
    FResourceNames: TList<string>;
    FSearchDictionary: TDictionary<string, string>;
    function GetFormClasses(AClass: TClass): TList<string>;
    procedure BuildStrings(AResources: TList<string>);
    procedure EnumClasses(AClass: TClass);
  public
    function Find(const ASearchString: string): TList<string>;
    constructor Create(AClass: TClass); overload;
    constructor Create(AClassNames: TList<string>); overload;
    destructor Destroy; override;
  end;

//  TProc
  TPropertyReloaderComparer = reference to function (const aCompClassName, aCompName, aPropName: string): Boolean;

  TPropertyReloader = class
  private
    FComparer: TPropertyReloaderComparer;
    procedure PropValueError;
    procedure ReadPropValue(aReader: TReader; const Instance: TPersistent; PropInfo: Pointer);
    procedure UpdateProperty(aReader: TReader; const AInstance: TPersistent; const aPropPath: string);
    procedure UpdateCollection(aReader: TReader; const Collection: TCollection);
  public
    procedure Process(aRoot: TComponent);
    procedure ProcessApp;

    constructor Create(aComparer: TPropertyReloaderComparer);

    class procedure Reload(aComparer: TPropertyReloaderComparer = nil);
  end;

  TLAReader = class(TReader)

  end;

  TLAPersistent = class(TPersistent)

  end;

implementation

uses
  System.TypInfo,
  System.RTLConsts,
  System.Rtti,
  System.StrUtils,
  FMX.Forms, FMX.Controls;

procedure ReadError(Ident: PResStringRec); overload;
begin
  raise EReadError.CreateRes(Ident) at ReturnAddress;
end;

procedure ReadError; overload;
begin
  raise EReadError.CreateRes(@SReadError) at ReturnAddress;
end;



function EnumResNames(Module: HMODULE; ResType, ResName: PChar; Obj: Pointer): integer; stdcall;
var
  ds: TDFMSearch;
begin
  ds := TDFMSearch(Obj);
  if ds.FFormClasses.IndexOf(string(ResName)) > -1 then
    ds.FResourceNames.Add(string(ResName));
  Result := 1;
end;


function TDFMSearch.GetFormClasses(AClass: TClass): TList<string>;
var
  cl: TClass;
  ctx: TRttiContext;
  types: TArray<TRttiType>;
  typ: TRttiType;
begin
  Result := TList<string>.Create;
  ctx := TRttiContext.Create;
  types := ctx.GetTypes;
  for typ in types do
  begin
    if typ.TypeKind = tkClass then
    begin
      cl := typ.AsInstance.MetaclassType;
      if (cl <> AClass) and cl.InheritsFrom(AClass) then
        Result.Add(UpperCase(cl.ClassName));
    end;
  end;
end;


procedure TDFMSearch.BuildStrings(AResources: TList<string>);
var
  i: integer;
  Flags: TFilerFlags;
  R: TReader;
  RS: Tresourcestream;
  vt: TValueType;
  Position: integer;
  text, PropName, CompClass, CompName, StrValue, ResStr: string;

  aComp: TComponent;

  function GetComp: TComponent;
  begin

  end;

  procedure ReadProperty;
  begin
    PropName := R.ReadStr;
//    if ContainsText(PropName, 'caption') then
    if ContainsText(PropName, 'color') then
    begin
      /// нужно найти компонент и установить его свойство
      ///  для этого нам нужно знать владельца
      vt := R.NextValue;
      case vt of
        vaWString, vaUTF8String:
          StrValue := R.ReadString;
        vaString, vaLString:
          StrValue := R.ReadString;
      else
        R.SkipValue;
      end;
      text := text + ' ' + StrValue;
    end
    else
      R.SkipValue;
  end;

procedure ReadData; forward;

  procedure ReadComponent;
  begin
    R.ReadPrefix(Flags, Position);
    CompClass := R.ReadStr;
    CompName := R.ReadStr;
    ReadData;
  end;

  procedure ReadData;
  begin
    while not R.EndOfList do
      ReadProperty;
    R.ReadListEnd;
    while not R.EndOfList do
      ReadComponent;
    R.ReadListEnd;
  end;


begin
  for i := 0 to AResources.Count - 1 do
  begin
    ResStr := UpperCase(AResources[i]);
    RS := TResourceStream.Create(HInstance, ResStr, RT_RCDATA);
    try
      R := TReader.Create(RS, 4096);
      try
        text := '';
        R.ReadSignature;
        ReadComponent;
        FSearchDictionary.AddOrSetValue(ResStr, text);
      finally
        R.Free;
      end;
    finally
      RS.Free;
    end;
  end;
end;


constructor TDFMSearch.Create(AClass: TClass);
begin
  FSearchDictionary := TDictionary<string, string>.Create;
  EnumClasses(AClass);
end;


constructor TDFMSearch.Create(AClassNames: TList<string>);
begin
  FSearchDictionary := TDictionary<string, string>.Create;
  BuildStrings(AClassNames);
end;


destructor TDFMSearch.Destroy;
begin
  FSearchDictionary.Free;
  inherited;
end;


procedure TDFMSearch.EnumClasses(AClass: TClass);
begin
  //if not Assigned(FSearchDictionary) then
  begin
    FFormClasses := GetFormClasses(AClass);
    try
      FResourceNames := TList<string>.Create;
      try
        //EnumResourceNames(HInstance, RT_RCDATA, @EnumResNames, NativeInt(Self));
        FResourceNames.Add(FFormClasses[0]);
        BuildStrings(FResourceNames);
      finally
        FResourceNames.Free;
      end;
    finally
      FFormClasses.Free;
    end;
  end;
end;


function TDFMSearch.Find(const ASearchString: string): TList<string>;
var
  i, j, slen: integer;
  sl: TArray<string>;
  pa: TPair<string, string>;
begin
  Result := TList<string>.Create;
  sl := ASearchString.Split([' ']);
  slen := Length(sl);
  for pa in FSearchDictionary do
  begin
    j := 0;
    for i := 0 to High(sl) do
    begin
      if ContainsText(pa.Value, sl[i]) then
        inc(j);
    end;
    if j = slen then
      Result.Add(pa.Key);
  end;
end;

{ TPropertyReloader }

constructor TPropertyReloader.Create(aComparer: TPropertyReloaderComparer);
begin
  inherited Create;
  FComparer := aComparer;
end;

procedure TPropertyReloader.Process(aRoot: TComponent);
var
  ResStr: string;
  RS: TResourceStream;
  R: TReader;
  Flags: TFilerFlags;
  vt: TValueType;
  Position: Integer;
  PropName, CompClass, CompName, StrValue: string;
  Comp: TComponent;

  procedure ReadProperty;
  begin
    PropName := R.ReadStr;
    UpdateProperty(R, Comp, PropName)
  end;

  procedure ReadData(aLookupRoot: TComponent); forward;

  procedure ReadComponent(aLookupRoot: TComponent);
  begin
    R.ReadPrefix(Flags, Position);
    CompClass := R.ReadStr;
    CompName := R.ReadStr;
    if aLookupRoot.Name = CompName then
      Comp := aLookupRoot
    else
    begin
      Comp := aLookupRoot.FindComponent(CompName);
      if ffInline in Flags then
        aLookupRoot := Comp;
    end;

    if Comp = nil then
      Comp := aLookupRoot;

    ReadData(aLookupRoot);
  end;

  procedure ReadData(aLookupRoot: TComponent);
  begin
    while not R.EndOfList do
      ReadProperty;
    R.ReadListEnd;
    while not R.EndOfList do
      ReadComponent(aLookupRoot);
    R.ReadListEnd;
  end;

begin
  ResStr := UpperCase(aRoot.ClassName);
  RS := TResourceStream.Create(HInstance, ResStr, RT_RCDATA);
  try
    R := TReader.Create(RS, 4096);
    try
      R.ReadSignature;
      ReadComponent(aRoot);
    finally
      R.Free;
    end;
  finally
    RS.Free;
  end;

end;

procedure TPropertyReloader.ProcessApp;
var
  aCompToProcess: TList<TComponent>;
  aFrame: TFrame;
  aComp: TComponent;

  procedure AddSubFramesAndOwer(aOwner: TComponent);
  begin
    for var i := 0 to aOwner.ComponentCount - 1 do
    begin
      aComp := aOwner.Components[i];
      if (aComp is TFrame) and (aCompToProcess.IndexOf(aComp) < 0) then
        AddSubFramesAndOwer(aComp)
      else if (aComp is TDataModule) and (aCompToProcess.IndexOf(aComp) < 0) then
        aCompToProcess.Add(aComp);

    end;
    if aCompToProcess.IndexOf(aOwner) < 0 then
      aCompToProcess.Add(aOwner);
  end;
begin
  /// нужно обработать все открытые формы и встроенные в них фреймы
  ///  - для каждой формы
  ///    - находим встроенные в нее фреймы
  ///    - каждый фрейм может содержать другой фрейм, добавляем встроенные фреймы рекурсивно
  ///    - соблюдаем порядок: первым идет самый крайний встроенный фрейм, последней форма
  ///  - проходим по добавленным фреймам и формам

  aCompToProcess := TList<TComponent>.Create;
  try
    for var i := 0 to Screen.FormCount - 1 do
      AddSubFramesAndOwer(Screen.Forms[i]);

    for var i := 0 to aCompToProcess.Count - 1 do
    begin
//      if aCompToProcess.Items[i] is TControl then
//        TControl(aCompToProcess.Items[i]).BeginUpdate;
      try
        Process(aCompToProcess.Items[i]);
      finally
//        if aCompToProcess.Items[i] is TControl then
//          TControl(aCompToProcess.Items[i]).EndUpdate;
      end;
    end;

  finally
    aCompToProcess.Free;
  end;


end;

procedure TPropertyReloader.PropValueError;
begin
  ReadError(@SInvalidPropertyValue);
end;

procedure TPropertyReloader.UpdateCollection(aReader: TReader; const Collection: TCollection);
var
  i: Integer;
  Item: TPersistent;
begin
  Collection.BeginUpdate;
  try
    //if not aReader.EndOfList then Collection.Clear;
    i := 0;
    while not aReader.EndOfList do
    begin
      if aReader.NextValue in [vaInt8, vaInt16, vaInt32] then aReader.ReadInteger;
      //Item := Collection.Add;
      Item := Collection.Items[i];
      aReader.ReadListBegin;
      while not aReader.EndOfList do
      begin
        //ReadProperty(Item);
        UpdateProperty(aReader, Item, aReader.ReadStr);
      end;
      aReader.ReadListEnd;
      Inc(i);
    end;
    aReader.ReadListEnd;
  finally
    Collection.EndUpdate;
  end;
end;

procedure TPropertyReloader.ReadPropValue(aReader: TReader; const Instance: TPersistent; PropInfo: Pointer);
const
  NilMethod: TMethod = (Code: nil; Data: nil);
var
  PropType: PTypeInfo;
  LMethod: TMethod;
  EnumValue: Integer;

  procedure SetIntIdent(const Instance: TPersistent; PropInfo: Pointer;
    const Ident: string);
  var
    V: FixedInt;
    IdentToInt: TIdentToInt;
  begin
    IdentToInt := FindIdentToInt(PPropInfo(PropInfo)^.PropType^);
    if Assigned(IdentToInt) and IdentToInt(Ident, V) then
      SetOrdProp(Instance, PropInfo, V)
    else
      PropValueError;
  end;

//  procedure SetObjectIdent(const Instance: TPersistent; PropInfo: Pointer;
//    const Ident: string);
//  begin
//    FFixups.Add(TPropFixup.Create(Instance, Root, PropInfo, '', Ident));
//  end;

  // This is isolated into a local to help reduce transient VarClears
  procedure SetVariantReference;
  begin
    SetVariantProp(Instance, PropInfo, aReader.ReadVariant);
  end;

//  procedure SetInterfaceReference;
//  var
//    Intf: IInterface;
//  begin
//    if NextValue = vaNil then
//    begin
//      ReadValue;
//      Intf := nil;
//      SetInterfaceProp(Instance, PropInfo, Intf);
//    end
//    else
//      FFixups.Add(TPropIntfFixup.Create(Instance, Root, PropInfo, '', ReadIdent));
//  end;

begin
  if PPropInfo(PropInfo)^.SetProc = nil then
    if not ((PPropInfo(PropInfo)^.PropType^.Kind = tkClass) and
       (TObject(GetOrdProp(Instance, PropInfo)) is TComponent) and
       (csSubComponent in TComponent(GetOrdProp(Instance, PropInfo)).ComponentStyle)) then
      ReadError(@SReadOnlyProperty);
  PropType := PPropInfo(PropInfo)^.PropType^;

  // у нас есть полная информаця о свойстве, можем проверить, нужно ли его обновлять
  if not ((PropType^.Kind in [tkClass]) or ContainsText(PropType^.Name, 'color')) then
  begin
    aReader.SkipValue;
    Exit;
  end;

  case PropType^.Kind of
    tkInteger:
      if aReader.NextValue = vaIdent then
        SetIntIdent(Instance, PropInfo, aReader.ReadIdent)
      else
        SetOrdProp(Instance, PropInfo, aReader.ReadInteger);
    tkChar, tkWChar:
      SetOrdProp(Instance, PropInfo, Ord(aReader.ReadChar));
    tkEnumeration:
      begin
        EnumValue := GetEnumValue(PropType, aReader.ReadIdent);

        if (EnumValue <> -1) or
           // Longbool/wordbool/bytebool
           IsBoolType(PropType) and (GetTypeData(PropType)^.MinValue < 0) then
          SetOrdProp(Instance, PropInfo, EnumValue)
        else
          PropValueError;
      end;
    tkFloat:
      SetFloatProp(Instance, PropInfo, aReader.ReadFloat);
    tkString, tkLString:
      SetStrProp(Instance, PropInfo, aReader.ReadString);
    tkWString:
      SetStrProp(Instance, PropInfo, aReader.ReadString);
    tkUString:
      SetStrProp(Instance, PropInfo, aReader.ReadString);
    tkSet:
      //aReader.SkipValue; // SetOrdProp(Instance, PropInfo, aReader.ReadSet(PropType));
      SetOrdProp(Instance, PropInfo, TLAReader(aReader).ReadSet(PropType));
    tkClass:
      case aReader.NextValue of
        vaNil:
          begin
            aReader.ReadValue;
            SetOrdProp(Instance, PropInfo, 0);
          end;
        vaCollection:
          begin
            aReader.ReadValue;
            //aReader.ReadCollection(TCollection(GetOrdProp(Instance, PropInfo)));
            UpdateCollection(aReader, TCollection(GetOrdProp(Instance, PropInfo)));
          end
      else
        //aReader.ReadIdent;
        aReader.SkipValue;  //SetObjectIdent(Instance, PropInfo, ReadIdent);
      end;
    tkMethod:
      if aReader.NextValue = vaNil then
      begin
        aReader.ReadValue;
        SetMethodProp(Instance, PropInfo, NilMethod);
      end
      else
      begin
        aReader.SkipValue;
        //aReader.ReadIdent;
//        LMethod := aFindMethodInstance(Root, ReadIdent);
//        if LMethod.Code <> nil then SetMethodProp(Instance, PropInfo, LMethod);
      end;
    tkVariant:
      SetVariantReference;
    tkInt64:
      SetInt64Prop(Instance, PropInfo, aReader.ReadInt64);
    tkInterface:
      aReader.SkipValue; //SetInterfaceReference;
  end;
end;

class procedure TPropertyReloader.Reload(aComparer: TPropertyReloaderComparer);
begin
  var r := TPropertyReloader.Create(aComparer);
  try
    r.ProcessApp;
  finally
    r.Free;
  end;
end;

procedure TPropertyReloader.UpdateProperty(aReader: TReader; const AInstance: TPersistent; const aPropPath: string);
var
  I, J, L: Integer;
  Instance: TPersistent;
  PropInfo: PPropInfo;
  PropValue: TObject;
  PropPath: string;
  PropName: string;

//  procedure HandleException(E: Exception);
//  var
//    Name: string;
//  begin
//    Name := '';
//    if AInstance is TComponent then
//      Name := TComponent(AInstance).Name;
//    if Name = '' then Name := AInstance.ClassName;
//    raise EReadError.CreateResFmt(@SPropertyException, [Name, DotSep, PropPath, E.Message]);
//  end;

  procedure PropPathError;
  begin
    aReader.SkipValue;
    //aReader.ReadError(@SInvalidPropertyPath);
  end;

  procedure PropertyError(const Name: string);
  begin
    aReader.SkipValue;
    //raise EReadError.CreateResFmt(@SUnknownProperty, [Name]);
  end;


begin
  try
    PropPath := aPropPath; //ReadStr;
    try
      I := Low(string);
      L := High(PropPath);
      Instance := AInstance;
//      FCanHandleExcepts := True;
      while True do
      begin
        J := I;
        while (I <= L) and (PropPath[I] <> '.') do Inc(I);
        PropName := PropPath.SubString(J - Low(string), I - J);
        if I > L then Break;
        PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
        if PropInfo = nil then
        begin
//          // Call DefineProperties with the entire PropPath
//          // to allow defining properties such as "Prop.SubProp"
//          FPropName := PropPath;
//          { Cannot reliably recover from an error in a defined property }
//          FCanHandleExcepts := False;
//          Instance.DefineProperties(Self);
//          FCanHandleExcepts := True;
          if PropName <> '' then
            PropertyError(PropName);
          Exit;
        end;
        PropValue := nil;
        if PropInfo^.PropType^.Kind = tkClass then
          PropValue := TObject(GetOrdProp(Instance, PropInfo));
        if not (PropValue is TPersistent) then PropPathError;
        Instance := TPersistent(PropValue);
        Inc(I);
      end;
      PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
//      PropInfo := GetPropInfo(Instance, PropName);
      if PropInfo <> nil then
        ReadPropValue(aReader, Instance, PropInfo)
      else
      begin
//        { Cannot reliably recover from an error in a defined property }
//        FCanHandleExcepts := False;
        TLAPersistent(Instance).DefineProperties(aReader);

//        FCanHandleExcepts := True;
        if PropName <> '' then
        begin
          // попробуем еще раз
          PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
          if PropInfo <> nil then
            ReadPropValue(aReader, Instance, PropInfo)
          else
            PropertyError(PropName);
        end;
      end;
    except
      raise;
      //on E: Exception do HandleException(E);
    end;
  except
    on E: Exception do
      raise;
//      if not FCanHandleExcepts or not Error(E.Message) then raise;
  end;
end;

end.
