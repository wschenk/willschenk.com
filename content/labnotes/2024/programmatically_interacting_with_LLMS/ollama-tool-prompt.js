export default function makePrompt(tools) {
    const toolInfo = JSON.stringify( tools, null, "  " );
    return `

You have access to the following tools:
{${toolInfo}}

You must follow these instructions:
You must return valid JSON.
Always select one or more of the above tools based on the user query
If a tool is found, you must respond in the JSON format matching the following schema:
{{
   "tools": {{
        "tool": "<name of the selected tool>",
        "tool_input": <parameters for the selected tool, matching the tool's JSON schema
   }}
}}
If there are multiple tools required, make sure a list of tools are returned in a JSON array.
If there is no tool that match the user request, you will respond with empty json.
Do not add any additional Notes or Explanations

User Query:`
}
