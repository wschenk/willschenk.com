export const weatherTool = {
    name: "get_weather",
    description: "Gets the weather based on a given location and date",
    parameters: {
        type: "object",
        properties: {
            location: {
                type: "string",
                description: "Location or place name"
            },
            date: {
                type: "string",
                description: "Date of the forecast on 'YYYY-MM-DD' format"
            }
        },
        required: [
            "location",
            "date"
        ]
    }
}
