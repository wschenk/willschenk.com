export const distanceTool = {
    name: "get_distance",
    description: "Gets the driving distance between two locations",
    parameters: {
        type: "object",
        properties: {
            start: {
                type: "string",
                description: "Location or place name where you are starting"
            },
            destination: {
                type: "string",
                description: "Location or place name of destination"
            }
        },
        required: [
            "start",
            "destination"
        ]
    }
}
