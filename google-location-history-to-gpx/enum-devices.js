import fs from 'fs';
import path from 'path';
import JSONStream from 'JSONStream';

const inputFile = "./takeout-20211217T164337Z-001/Takeout/ロケーション履歴/ロケーション履歴.json"; //[修正してください]

const devices = [];

function recordDevice(data){
    const device = devices.find((device)=>device.tag == data.deviceTag);
    const time = (new Date(parseInt(data.timestampMs))).toISOString();
    if(device){
        device.lastTime = time;
        device.count++;
        if(data.latitudeE7 < device.latMin){
            device.latMin = data.latitudeE7;
        }
        if(data.latitudeE7 > device.latMax){
            device.latMax = data.latitudeE7;
        }
        if(data.longitudeE7 < device.lngMin){
            device.lngMin = data.longitudeE7;
        }
        if(data.longitudeE7 > device.lngMax){
            device.lngMax = data.longitudeE7;
        }
    }
    else{
        devices.push({
            tag:data.deviceTag,
            firstTime: time,
            lastTime: time,
            latMin: data.latitudeE7,
            latMax: data.latitudeE7,
            lngMin: data.longitudeE7,
            lngMax: data.longitudeE7,
            count: 1
        });
    }
}
function showDevices(){
    console.log(devices);
}

const inputStream = fs.createReadStream(inputFile)
      .pipe(JSONStream.parse('locations.*'));

inputStream.on('data', (data)=>{
    recordDevice(data);
}).on('end', ()=>{
    showDevices();
});
