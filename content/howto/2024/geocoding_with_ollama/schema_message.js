const schema = {
    city: {
        type: "string",
        description: "name of city",
    },
    state: {
        type: "string",
        description: "state of provence of the city"
    },
    country: {
        type: "string",
        description: "country of the city"
    },
    population: {
        type: "string",
        description: "population of the city",
        },
    description: {
        type: "string",
        description: "description of the city"
    },
    lat: {
        type: "float",
        description: "decimal latitude of the city"
    },
    lon: {
        type: "float",
        description: "decimal longitude of the city"
    }
}
        

export default function schema_message( city ) {
    return {
        model: "Mistral:7b",
        messages: [
    { "role": "user", content:
      `where is the city ${city} in north america,
describe the city as description, and where
is it located in latitude and longitude
in decimal.  output in json using the schema
defined here ${JSON.stringify( schema )}` }
        ],
        format: "json"
    }
}
