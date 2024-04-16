export const clarifyTool = {
    name: "clarify",
    descriptions: "Asks the user for clarifying information to feed into a tool",
    parameters: {
        type: "object",
        properties: {
            information: {
                type: "string",
                description: "A descriptions of what further information required"
            }
        }
    }
}
