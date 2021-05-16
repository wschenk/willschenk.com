package dockeringo

import (
  "context"
  "io"
  "time"

  "github.com/docker/docker/api/types"
)

func (c *Controller) ContainerLog(id string) (result string, err error) {
  ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
  defer cancel()

  reader, err := c.cli.ContainerLogs(ctx, id, types.ContainerLogsOptions{
    ShowStdout: true,
    ShowStderr: true})

  if err != nil {
    return "", err
  }

  buffer, err := io.ReadAll(reader)

  if err != nil && err != io.EOF {
    return "", err
  }

  return string(buffer), nil
}
