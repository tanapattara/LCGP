const { Client, Status } = require("@googlemaps/google-maps-services-js");
const fs = require("fs");

const client = new Client({});
client
  .distancematrix({
    params: {
      //origins: ["Seattle, WA"],
      //destinations: ["San Francisco, CA"],
      origins: [{ lat: 45, lng: -110 }],
      destinations: [{ lat: 46, lng: -110 }],
      key: "API-KEY",
    },
    timeout: 1000, // milliseconds
  })
  .then((r) => {
    if (r.data.status === Status.OK) {
      var distance = r.data.rows[0].elements[0].distance.value; //meters
      var duration = r.data.rows[0].elements[0].duration.value; //seconds

      console.log("distance " + distance);
      console.log("duration " + duration);
    } else {
      console.log(r.data.error_message);
    }
  })
  .catch((e) => {
    console.log(e);
  });

// var result = "";
// var a = 10;
// var b = 20;
// var c = 30;
// var output = '${a}, ${b}, ${c}\r\n';
// result += output;
// result += output;

// // write file to
// fs.writeFile(__dirname + "/output/distance.csv", result, function (err) {
//   if (err) {
//     return console.log(err);
//   }
//   console.log("The file was saved!");
// });
