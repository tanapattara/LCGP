import * as dotenv from "dotenv";
dotenv.config();
import * as csv from "@fast-csv/parse";
import * as fastCsv from "fast-csv";
import fs from "fs-extra";
import * as path from "path";
import axios from "axios";
import querystring from "querystring";

interface Explore {
  venue_id: string;
  latitude: string;
  longitude: string;
  fq_id: string;
  name: string;
  category: string;
  primary: boolean;
}

interface Location {
  latitude: string;
  longitude: string;
}

interface RowData extends Location {
  venue_id: string;
}

const explore = async (
  venue_id: string,
  location: Location
): Promise<Explore | undefined> => {
  try {
    const queryString = querystring.stringify({
      client_id: process.env.CLIENT_ID as string,
      client_secret: process.env.CLIENT_SECRET as string,
      v: "20180323",
      limit: 1,
    });
    const {
      data: { response: data },
      status,
    } = await axios.get(
      `https://api.foursquare.com/v2/venues/explore?${queryString}&ll=${location.latitude},${location.longitude}`
    );
    if (status === 200) {
      const { groups } = data;
      if (groups!.length > 0 && groups[0]!.items!.length > 0) {
        const { name, id: fq_id, categories } = groups[0]!.items[0]!.venue;
        const { name: category, primary } = categories[0]!;
        const newItem = {
          venue_id,
          ...location,
          fq_id,
          name,
          category,
          primary,
        };
        return newItem;
      }
    } else {
      throw new Error("Fetch data error.");
    }
  } catch (err) {
    console.error(err);
  }
};

const readCsv = (): Promise<Array<RowData>> =>
  new Promise((resolve) => {
    const data: Array<RowData> = [];
    fs.createReadStream(path.resolve(__dirname, "..", "venue.csv"))
      .pipe(csv.parse({ headers: true }))
      .on("data", async (row: RowData) => {
        data.push(row);
      })
      .on("end", () => {
        resolve(data);
      });
  });

const bootstrap = async () => {
  const data = await readCsv();
  const fetchData: Array<Explore> = [];
  for await (const location of data) {
    const fetchingData = (await explore(location.venue_id, {
      latitude: location.latitude,
      longitude: location.longitude,
    })) as Explore;
    fetchData.push(fetchingData);
  }
  const writeStream = fs.createWriteStream(
    path.resolve(__dirname, "..", "explore_data.csv")
  );
  fastCsv.write(fetchData, { headers: true }).pipe(writeStream);
};

bootstrap();
