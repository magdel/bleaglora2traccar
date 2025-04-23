program bleaglora2traccar;

{$mode objfpc}{$H+}

{
  AGLoRa BLE data to Traccar resender.

  Use bledevice.ini to predefine deviceid and service so it will connect automatically on start.

  Based on Lazarus / Free Pascal BLE notify example for SimpleBLE library.

  Example is Copyright (c) 2022 Erik Lins and released under the MIT License.
  https://github.com/eriklins/Pascal-Bindings-For-SimpleBLE-Library

  The SimpleBLE library is Copyright (c) 2021-2022 Kevin Dewald and released under the MIT License.
    https://github.com/OpenBluetoothToolbox/SimpleBLE
}

{$UNDEF DYNAMIC_LOADING}
{$IFDEF WINDOWS}
  {$DEFINE DYNAMIC_LOADING}    { UNCOMMENT IF YOU WANT DYNAMIC LOADING }
{$ENDIF}

//{$DEFINE B2T_DEBUG_OUTPUT}    { UNCOMMENT IF YOU WANT OUTPUT TO CONSOLE FOR DEBUG PURPOSE }

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  strutils,
  DateUtils,
  CustApp,
  IdUDPClient,
  IniFiles,
  Contnrs,
  SerialStream,
  SimpleBle;

type

  { TBle2UdpApplication }

  TBle2UdpApplication = class(TCustomApplication)
  protected
    procedure RunInternal;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TServiceCharacteristic = record
    Service: TSimpleBleUuid;
    Characteristic: TSimpleBleUuid;
  end;

  { TReadThread }

  TReadThread = class(TThread)
  public

    procedure Execute; override;
  end;

const
  PERIPHERAL_LIST_SIZE = 128;
  SERVICES_LIST_SIZE = 64;


var
  CharacteristicList: array [0..SERVICES_LIST_SIZE - 1] of TServiceCharacteristic;
  PeripheralList: array [0..PERIPHERAL_LIST_SIZE - 1] of TSimpleBlePeripheral;
  PeripheralListLen: nativeuint = 0;
  Adapter: TSimpleBleAdapter = 0;

  ScanTimeoutMs: integer;
  DeviceIdFromConfig: string;
  CharacteristicFromConfig: string;
  PortFromConfig: string;
  HostFromConfig: string;
  BufferMemoryStream: TMemoryStream;
  BufferCriticalSection: TRTLCriticalSection;
  WaitForReadEvent: PRtlEvent;
  ReadThread: TReadThread;

  { Callback functions for SimpleBLE }

  procedure AdapterOnScanStart(Adapter: TSimpleBleAdapter; Userdata: PPointer);
  var
    Identifier: PChar;
  begin
    Identifier := SimpleBleAdapterIdentifier(Adapter);
    if Identifier = '' then
      Exit;
    WriteLn('Adapter ' + Identifier + ' started scanning.');
    SimpleBleFree(Identifier);
  end;

  procedure AdapterOnScanStop(Adapter: TSimpleBleAdapter; Userdata: PPointer);
  var
    Identifier: PChar;
  begin
    Identifier := SimpleBleAdapterIdentifier(Adapter);
    if Identifier = '' then
      Exit;
    WriteLn('Adapter ' + Identifier + ' stopped scanning.');
    SimpleBleFree(Identifier);
  end;

  procedure AdapterOnScanFound(Adapter: TSimpleBleAdapter;
    Peripheral: TSimpleBlePeripheral; Userdata: PPointer);
  var
    AdapterIdentifier: PChar;
    PeripheralIdentifier: PChar;
    PeripheralAddress: PChar;
  begin
    AdapterIdentifier := SimpleBleAdapterIdentifier(adapter);
    PeripheralIdentifier := SimpleBlePeripheralIdentifier(peripheral);
    PeripheralAddress := SimpleBlePeripheralAddress(peripheral);
    if (AdapterIdentifier = '') or (PeripheralAddress = '') then
      Exit;
    WriteLn('Adapter ' + AdapterIdentifier + ' found device: ' +
      PeripheralIdentifier + ' [' + PeripheralAddress + ']');
    if PeripheralListLen < PERIPHERAL_LIST_SIZE then
    begin
      // Save the peripheral
      PeripheralList[PeripheralListLen] := peripheral;
      Inc(PeripheralListLen);
    end
    else
    begin
      // As there was no space left for this peripheral, release the associated handle.
      SimpleBlePeripheralReleaseHandle(peripheral);
    end;
    SimpleBleFree(AdapterIdentifier);
    SimpleBleFree(PeripheralIdentifier);
    SimpleBleFree(PeripheralAddress);
  end;

  procedure AdapterOnScanFoundUpdated(Adapter: TSimplebleAdapter;
    Peripheral: TSimpleBlePeripheral; Userdata: PPointer);
  var
    AdapterIdentifier: PChar;
    PeripheralAddress: PChar;
    DevIdx, j, k: integer;
    FlagNewData: boolean;
    s: string;
    TmpManufacturerData: TSimpleBleManufacturerData;
  begin
    //do nothing
  end;

  procedure PeripheralOnNotify(Service: TSimpleBleUuid;
    Characteristic: TSimpleBleUuid; Data: pbyte; DataLength: nativeuint;
    Userdata: PPointer);
  var
    i: integer;
  begin

    {$IFDEF B2T_DEBUG_OUTPUT}
    for i := 0 to (DataLength - 1) do
      Write(ansichar(Data[i]));
    {$ENDIF}

    EnterCriticalsection(BufferCriticalSection);
    try
      BufferMemoryStream.Write(Data[0], DataLength);

    finally
      LeaveCriticalsection(BufferCriticalSection);
    end;
    RtlEventSetEvent(WaitForReadEvent);
  end;

  { TReadThread }

  procedure fillDataMap(map: TFPStringHashTable; Data: unicodestring);
  var
    SL: TStringList;
    I, eqIndex: integer;
    s, k: string;
  begin
    SL := TStringList.Create;
    try
      SL.Delimiter := '&';
      SL.DelimitedText := Data;
      for I := 0 to SL.Count - 1 do
      begin
        s := SL[I];
        eqIndex := Pos('=', s);
        if (eqIndex > 1) then
        begin
          k := Copy2SymbDel(s, '=');
          map[k] := s;
        end;
      end;
    finally
      SL.Free;
    end;
  end;

  function formatTraccarString(map: TFPStringHashTable): ansistring;
  var
    ts: string;
    dt: TDateTime;
  begin
    Result := '|unit=' + map['n'] + ',unittype=1,' + 'longitude=' +
      map['o'] + ',' + 'latitude=' + map['a'] + ',' + 'altitude=' +
      map['h'] + ',gps_valid=1,' + 'velocity=' + Copy2Symb(map['s'], '.') +
      ',' + 'heading=' + map['c'] + ',';
    if (map.Find('e_id') <> nil) then
      Result += 'eventid=' + map['e_id'] + ',';
    if (map.Find('sat') <> nil) then
      Result += 'satellites=' + map['sat'] + ',';
    if (map.Find('u') <> nil) then
      Result += 'status=' + map['u'] + ',';
    if (map.Find('b') <> nil) then
      Result += 'batteryLevel=' + map['b'] + ',';
    //some custom part
    if (map.Find('t_id1') <> nil) then
      Result += 't_id1=' + map['t_id1'] + ',';
    if (map.Find('t_bat1') <> nil) then
      Result += 't_bat1=' + map['t_bat1'] + ',';
    if (map.Find('t_id2') <> nil) then
      Result += 't_id2=' + map['t_id2'] + ',';
    if (map.Find('t_bat2') <> nil) then
      Result += 't_bat2=' + map['t_bat2'] + ',';

    //datetime reformat
    dt := ISO8601ToDate(map['t'], True);
    ts := FormatDateTime('yyyy"/"mm"/"dd HH":"nn":"ss', dt);
    Result += 'datetime_actual=' + ts + '|'#13#10;
  end;

  procedure TReadThread.Execute;
  var
    readCount: longint;
    Utf8Data: unicodestring;
    byteBuffer: array of byte;
    I: integer;
    map: TFPStringHashTable;
    udp: TIdUDPClient;
    traccarData: ansistring;
  begin
    map := TFPStringHashTable.Create;
    udp := TIdUDPClient.Create();
    udp.Host := HostFromConfig;
    udp.Port := StrToInt(PortFromConfig);
    try
      while True do
      begin
        RtlEventWaitFor(WaitForReadEvent);

        EnterCriticalsection(BufferCriticalSection);
        try
          readCount := 0;
          for I := 0 to BufferMemoryStream.Size - 2 do
          begin
            if ((TBytes(BufferMemoryStream.Memory)[I] = 13) and
              (TBytes(BufferMemoryStream.Memory)[I + 1] = 10)) then
            begin
              SetLength(byteBuffer, I);
              if (Length(byteBuffer) > 0) then
              begin
                //has some to read, not only CRLF
                BufferMemoryStream.Position := 0;
                readCount := BufferMemoryStream.Read(byteBuffer[0], Length(byteBuffer));
              end;

              if (I < BufferMemoryStream.Size - 2) then
              begin
                //move some after CRLF to start
                Move(TBytes(BufferMemoryStream.Memory)[I + 2], TBytes(
                  BufferMemoryStream.Memory)[0], I + 2);
                BufferMemoryStream.SetSize(BufferMemoryStream.Size - I - 2);
                BufferMemoryStream.Position := BufferMemoryStream.Size;
              end
              else
              begin
                //or just clear all because we just read it
                BufferMemoryStream.SetSize(0);
              end;
            end;
          end;
        finally
          LeaveCriticalsection(BufferCriticalSection);
        end;
        if (readCount > 0) then
        begin
          //create string
          Utf8Data := UTF8ToString(byteBuffer);
          WriteLn('Read line(' + IntToStr(readCount) + '), utf8: ' + Utf8Data);
          if (StartsStr('AGLoRaN', Utf8Data)) then
          begin
            //parse string
            map.Clear;
            try
              fillDataMap(map, Utf8Data);
              //format udp string
              traccarData := formatTraccarString(map);
              //send udp string
              udp.Send(traccarData);

              WriteLn('UDP: sent(' + IntToStr(Length(traccarData)) + ') some: ' + traccarData);
            except
              on E: Exception do
              begin
                WriteLn('Error: ' + E.Message);
              end;
            end;
          end;
        end;
      end;
    finally
      map.Free;
      udp.Free;
    end;
  end;


  { -------------------------------- }
  procedure TBle2UdpApplication.DoRun;
  begin
    try
      RunInternal;
    except
      on E: Exception do
      begin
        WriteLn('Internal error: ' + E.Message);
      end;
    end;

  end;

  procedure TBle2UdpApplication.RunInternal;
  var
    Adapter: TSimpleBleAdapter;
    ErrCode: TSimpleBleErr = SIMPLEBLE_SUCCESS;
    i, j, Selection, CharacteristicCount: integer;
    Peripheral: TSimpleBlePeripheral;
    PeripheralIdentifier: PChar;
    PeripheralAddress: PChar;
    Service: TSimpleBleService;
  begin

    {$IFDEF DYNAMIC_LOADING}
  if not SimpleBleLoadLibrary() then begin
    WriteLn('Failed to load library simpleble.dll and simpleble-c.dll');
    ReadLn;
    exit;
  end;
    {$ENDIF}

    // look for BLE adapters
    if SimpleBleAdapterGetCount() = 0 then
    begin
      WriteLn('No BLE adapter was found.');
      Terminate;
      Exit;
    end;

    // get a handle for the BLE Adapter
    Adapter := SimpleBleAdapterGetHandle(0);
    if Adapter = 0 then
    begin
      WriteLn('Could not get handle for BLE adapter.');
      Terminate;
      Exit;
    end;
    WriteLn('Found BLE adapter and got handle. Scanning for ' + IntToStr(
      ScanTimeoutMs) + 'ms..');

    // register SimpleBLE scan callback functions
    SimpleBleAdapterSetCallbackOnScanStart(Adapter, @AdapterOnScanStart, nil);
    SimpleBleAdapterSetCallbackOnScanStop(Adapter, @AdapterOnScanStop, nil);
    SimpleBleAdapterSetCallbackOnScanFound(Adapter, @AdapterOnScanFound, nil);
    SimpleBleAdapterSetCallbackOnScanUpdated(Adapter, @AdapterOnScanFoundUpdated, nil);

    // start BLE scanning for 5 seconds
    SimpleBleAdapterScanFor(Adapter, ScanTimeoutMs);

    if (PeripheralListLen = 0) then
    begin
      WriteLn('No devices were found, exiting');
      Terminate;
      Exit;
    end;
    // list found Peripheral devices
    WriteLn('The following devices were found:');
    for i := 0 to (PeripheralListLen - 1) do
    begin
      Peripheral := PeripheralList[i];
      PeripheralIdentifier := SimpleBlePeripheralIdentifier(Peripheral);
      PeripheralAddress := SimpleBlePeripheralAddress(Peripheral);
      WriteLn('[' + IntToStr(i) + '] ' + PeripheralIdentifier + ' [' +
        PeripheralAddress + ']');
      SimpleBleFree(PeripheralIdentifier);
      SimpleBleFree(PeripheralAddress);
    end;

    // if we have device in config
    Selection := -1;
    if (DeviceIdFromConfig <> 'None') then
    begin

      for i := 0 to (PeripheralListLen - 1) do
      begin
        Peripheral := PeripheralList[i];
        PeripheralIdentifier := SimpleBlePeripheralIdentifier(Peripheral);
        PeripheralAddress := SimpleBlePeripheralAddress(Peripheral);
        if PeripheralAddress = DeviceIdFromConfig then
        begin
          Selection := i;
          WriteLn('Selected: ' + IntToStr(Selection));
          break;
        end;
      end;
    end;

    // select device to connect
    if (Selection = -1) then
    begin
      Selection := -1;
      Write('Please select a device to connect to: ');
      ReadLn(Selection);
      if (Selection < 0) or (Selection >= PeripheralListLen) then
      begin
        WriteLn('Invalid selection.');
        Terminate;
      end;
    end;

    // connect to selected device
    Peripheral := PeripheralList[Selection];
    PeripheralIdentifier := SimpleBlePeripheralIdentifier(Peripheral);
    PeripheralAddress := SimpleBlePeripheralAddress(Peripheral);
    WriteLn('Connecting to ' + PeripheralIdentifier + ' [' + PeripheralAddress + ']');
    SimpleBleFree(PeripheralIdentifier);
    SimpleBleFree(PeripheralAddress);
    ErrCode := SimpleBlePeripheralConnect(Peripheral);
    if ErrCode <> SIMPLEBLE_SUCCESS then
    begin
      WriteLn('Failed to connect.');
      Terminate;
    end;
    WriteLn('Successfully connected, listing services and characteristics.');

    // show list of characteristics to select one to subscribe to notifications
    CharacteristicCount := 0;
    for i := 0 to (SimpleBlePeripheralServicesCount(Peripheral) - 1) do
    begin
      ErrCode := SimpleBlePeripheralServicesGet(Peripheral, i, Service);
      if ErrCode <> SIMPLEBLE_SUCCESS then
      begin
        WriteLn('Failed to get service.');
        Terminate;
      end;
      for j := 0 to (Service.CharacteristicCount - 1) do
      begin
        if CharacteristicCount >= SERVICES_LIST_SIZE then
          break;
        WriteLn('[' + IntToStr(CharacteristicCount) + '] ' +
          Service.Uuid.Value + ' ' + Service.Characteristics[j].Uuid.Value);
        CharacteristicList[CharacteristicCount].Service := Service.Uuid;
        CharacteristicList[CharacteristicCount].Characteristic :=
          Service.Characteristics[j].Uuid;
        Inc(CharacteristicCount);
      end;
    end;

    Selection := -1;
    if (CharacteristicFromConfig <> 'None') then
    begin

      for i := 0 to (CharacteristicCount - 1) do
      begin

        if CharacteristicList[i].Characteristic.Value = CharacteristicFromConfig then
        begin
          Selection := i;
          WriteLn('Selected: ' + IntToStr(Selection));
          break;
        end;

      end;

      if (Selection = -1) then
      begin
        // select characteristic to subsribe notifications
        Write('Please select characteristic to read from: ');
        ReadLn(Selection);
        if (Selection < 0) or (Selection >= CharacteristicCount) then
        begin
          WriteLn('Invalid selection.');
          Terminate;
        end;
      end;

    end;

    BufferMemoryStream := TMemoryStream.Create;
    FreeAndNil(ReadThread);
    ReadThread := TReadThread.Create(True);
    ReadThread.Start;

    // subscribe to notification and register callback function
    SimpleBlePeripheralNotify(Peripheral, CharacteristicList[Selection].Service,
      CharacteristicList[Selection].Characteristic, @PeripheralOnNotify, nil);

    WriteLn('Listening until application is closed..');
    while (True) do
    begin
      Sleep(60000);
      WriteLn('Listening.. currentTime=' + DateTimeToStr(Now));
    end;

    // unsubscribe notifications
    SimpleBlePeripheralUnsubscribe(Peripheral, CharacteristicList[Selection].Service,
      CharacteristicList[Selection].Characteristic);

    // disconnect from Peripheral
    SimpleBlePeripheralDisconnect(Peripheral);
    //end;

    // wait for enter
    ReadLn();

    // release the BLE handle
    SimpleBleAdapterReleaseHandle(Adapter);

    {$IFDEF DYNAMIC_LOADING}
  SimpleBleUnloadLibrary();
    {$ENDIF}

    // stop program loop
    Terminate;
  end;

  constructor TBle2UdpApplication.Create(TheOwner: TComponent);
  var
    iniF: TIniFile;
  begin
    inherited Create(TheOwner);
    StopOnException := True;
    iniF := TIniFile.Create('bledevice.ini');
    try
      ScanTimeoutMs := iniF.ReadInteger('BleDevice', 'scanTimeoutMs', 10000);
      DeviceIdFromConfig := iniF.ReadString('BleDevice', 'deviceId', 'None');
      CharacteristicFromConfig := iniF.ReadString('BleDevice', 'characteristic', 'None');
      PortFromConfig := iniF.ReadString('TraccarStarcom', 'port', '5190');
      HostFromConfig := iniF.ReadString('TraccarStarcom', 'host', 'localhost');
    finally
      iniF.Free;
    end;
    WriteLn('Device id: ' + DeviceIdFromConfig);
    WriteLn('Characteristic: ' + CharacteristicFromConfig);
    WriteLn('Traccar: ' + HostFromConfig + ':' + PortFromConfig);
    WriteLn();
    InitCriticalSection(BufferCriticalSection);
    WaitForReadEvent := RTLEventCreate;
  end;

  destructor TBle2UdpApplication.Destroy;
  var
    i: integer;
  begin
    WriteLn('Releasing allocated resources.');
    // Release all saved peripherals
    for i := 0 to (PeripheralListLen - 1) do
      SimpleBlePeripheralReleaseHandle(PeripheralList[i]);
    // Let's not forget to release the associated handle.
    SimpleBleAdapterReleaseHandle(Adapter);
    DoneCriticalsection(BufferCriticalSection);
    RTLEventDestroy(WaitForReadEvent);
    inherited Destroy;
  end;


var
  Application: TBle2UdpApplication;

  {$R *.res}

begin
  Application := TBle2UdpApplication.Create(nil);
  Application.Title := 'BleAglora2Traccar';
  Application.Run;
  Application.Free;
end.
