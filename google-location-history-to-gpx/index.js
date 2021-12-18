import fs from 'fs';
import path from 'path';
import JSONStream from 'JSONStream';

const inputFile = "./takeout-20211217T164337Z-001/Takeout/ロケーション履歴/ロケーション履歴.json"; //[修正してください]
const outputDir = "output";

// 無視するデバイスの deviceTag を指定する。
// 自宅に置きっぱなしのデバイスがあるとその位置も出力されてしまうので。
// 先に enum-devices.js で調べておく。
const ignoreDevices = [
    //[修正してください]
    // 例:
    // 12345678, //端末A
    // -23232323, //端末B
    // 35353535 //端末C
];

let lastDate = null;
const dayPoints = [];

function addPoint(timeMs, lat, lng){
    const date = new Date(timeMs);

    // date change?
    if(!lastDate ||
       lastDate.getFullYear() != date.getFullYear() ||
       lastDate.getMonth() != date.getMonth() ||
       lastDate.getDate() != date.getDate()){

        const lastPoint = dayPoints[dayPoints.length-1];//point or undefined
        // Include the first point of the next day.
        if(lastDate){
            dayPoints.push({date, lat, lng});
        }

        flushGPX();

        // Include the last point of the previous day.
        if(lastPoint){
            dayPoints.push(lastPoint);
        }
    }

    lastDate = date;
    dayPoints.push({date, lat, lng});
}

function flushGPX(){
    if(lastDate){
        if(dayPoints.length > 0){
            writeGPX(lastDate, dayPoints);
        }
        lastDate = null;
        dayPoints.splice(0);
    }
}

function writeGPX(date, points){
    const yyyymm = "" + date.getFullYear() +
          ("0" + (date.getMonth() + 1)).substr(-2);
    const directory = path.join(outputDir, yyyymm);
    const dd = ("0" + (date.getDate())).substr(-2);
    const filename = "" + yyyymm + dd + ".gpx";
    fs.mkdirSync(directory, {recursive:true});
    const stream = fs.createWriteStream(path.join(directory, filename));

    const gpx = `<?xml version="1.0" encoding="UTF-8"?>
<gpx xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://www.topografix.com/GPX/1/0" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
<trk>
<name>${date.toISOString().split("T")[0]}</name>
<trkseg>
${
points.map((point)=>
            `<trkpt lat="${point.lat}" lon="${point.lng}">
<time>${point.date.toISOString()}</time>
</trkpt>
`).join("")}</trkseg></trk></gpx>`;
    stream.end(gpx);
}

const inputStream = fs.createReadStream(inputFile)
      .pipe(JSONStream.parse('locations.*'));

inputStream.on('data', (data)=>{
    if(!ignoreDevices.includes(data.deviceTag)){
        addPoint(
            parseInt(data.timestampMs),
            parseInt(data.latitudeE7) / 10000000,
            parseInt(data.longitudeE7) / 10000000);
    }
}).on('end', ()=>{
    flushGPX();
});
