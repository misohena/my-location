// 履歴の中に登場するデバイスの一覧を作成するスクリプト

// deviceTag(数値)と現れる時間の範囲や緯度経度の範囲を求める。

//   node enum-devices.js > devices.txt

import fs from 'fs';
import path from 'path';
import JSONStream from 'JSONStream';

const inputFile = "./takeout-20241212T072446Z-001/Takeout/ロケーション履歴/ロケーション履歴（タイムライン）/Records.json"; //[修正してください]

const devices = [];

function recordDevice(data){
    const device = devices.find((device)=>device.tag == data.deviceTag);

    // 以前は整数値で入っていたが、今回はISO文字列になっていた
    // const time = (new Date(parseInt(data.timestampMs))).toISOString();
    const time = (new Date(Date.parse(data.timestamp))).toISOString();

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
