require("dotenv").config();

const { Client } = require("@googlemaps/google-maps-services-js");

const client = new Client({});
client
  .placesNearby({
    params: {
      fields: "place_id,name,rating,geometry,types",
      radius: 10,
      //locations: [{ lat: 45, lng: -110 }],
      location: "-33.8670522,151.1957362",
      key: process.env.GOOGLE_MAPS_API_KEY,
    },
    timeout: 10000, // milliseconds
  })
  .then((r) => {
    console.log(r.data);
    //console.log(r);
  })
  .catch((e) => {
    console.log("error:" + e.response.data.error_message);
    //console.log("error:" + e);
  });
