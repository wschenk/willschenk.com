import { OllamaEmbeddings } from "@langchain/community/embeddings/ollama"
import { Ollama } from "@langchain/community/llms/ollama";
import { Chroma } from "@langchain/community/vectorstores/chroma";
import { PromptTemplate } from "@langchain/core/prompts";
import { StringOutputParser } from "@langchain/core/output_parsers"

const collectionName = 'sofreshandclean';
const embeddingModel = 'nomic-embed-text';
const llmModel = 'llama3';

const ollamaLlm = new Ollama({
    model: llmModel
});

const ollamaEmbeddings = new OllamaEmbeddings({
    model: embeddingModel
});

const vectorStore = await Chroma.fromExistingCollection(
    ollamaEmbeddings, { collectionName }
  );

const chromaRetriever = vectorStore.asRetriever();

const userQuestion = "Do you have a watch app and how do I use it?"

const simpleQuestionPrompt = PromptTemplate.fromTemplate(`
For following user question convert it into a standalone question
{userQuestion}`);

const simpleQuestionChain = simpleQuestionPrompt
      .pipe(ollamaLlm)
      .pipe(new StringOutputParser())
      .pipe(chromaRetriever);

const documents = await simpleQuestionChain.invoke({
    userQuestion: userQuestion
});

console.log( documents )

//Utility function to combine documents
function combineDocuments(docs) {
    return docs.map((doc) => doc.pageContent).join('\n\n');
}

//Combine the results into a string
const combinedDocs = combineDocuments(documents);

const questionTemplate = PromptTemplate.fromTemplate(`
You are an expert in electric vehicles and transportation. Answer the below question using the context.
    Strictly use the context and answer in crisp and point to point.
    <context>
    {context}
    </context>

    question: {userQuestion}
`)

const answerChain = questionTemplate.pipe(ollamaLlm);

const llmResponse = await answerChain.invoke({
    context: combinedDocs,
    userQuestion: userQuestion
});

console.log(llmResponse);
