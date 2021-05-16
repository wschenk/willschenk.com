package dockeringo

import (
  "github.com/docker/docker/api/types"
  "github.com/docker/docker/client"
)

type Controller struct {
  cli *client.Client
}

type VolumeMount struct {
  HostPath string
  Volume   *types.Volume
}

func NewController() (c *Controller, err error) {
  c = new(Controller)

  c.cli, err = client.NewClientWithOpts(client.FromEnv)

  if err != nil {
    return nil, err
  }
  return c, nil
}
