// emails/template.jsx
import { Html,
         Heading,
         Container,
         Button } from "@react-email/components";
import { CodeBlock, dracula } from "@react-email/code-block";

const code = `export default async (req, res) => {
  try {
    const html = await renderAsync(
      EmailTemplate({ firstName: 'John' })
    );
    return NextResponse.json({ html });
  } catch (error) {
    return NextResponse.json({ error });
  }
}`;

export default function Email() {
  return (
    <Html>
        <Container>
            <Heading as="h1">This is an email</Heading>
          <Button href="https://willschenk.com"
                  style={{ background: "#000", color: "#fff", padding: "12px 20px" }}>
              Click me
          </Button>

           <CodeBlock
               code={code}
               lineNumbers
               theme={dracula}
               language="javascript"
           />

       </Container>
    </Html>
  );
};
