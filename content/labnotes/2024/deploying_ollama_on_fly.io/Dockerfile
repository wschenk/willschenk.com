# Stage 1: Build ollama service
FROM ollama/ollama:latest AS ollama

# Stage 2: Build ollama-webui service and copy everything from ollama
FROM ghcr.io/open-webui/open-webui:latest

COPY --from=ollama / /

WORKDIR /app/backend
ENV OLLAMA_BASE_URL 'http://localhost:11434'
RUN ln -s /app/backend/data /root/.ollama
COPY both_start.sh ./
RUN chmod +x both_start.sh

CMD ["./both_start.sh"]
