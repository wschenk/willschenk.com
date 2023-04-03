package dockeringo

import "testing"

func TestPersistentVolume(t *testing.T) {
	c, err := NewController()

	if err != nil {
		t.Error(err)
		t.FailNow()
	}

	created, volume, err := c.EnsureVolume("persistentvolume")

	if err != nil {
		t.Error(err)
		t.FailNow()
	}

	if created != true {
		t.Errorf("Should have created a volume at the start")
	}

	mounts := []VolumeMount{
		{
			HostPath: "/volume",
			Volume:   volume,
		},
	}

	statusCode, body1, err := c.ContainerRunAndClean("testimage", []string{}, mounts)

	// Second run

	statusCode, body2, err := c.ContainerRunAndClean("testimage", []string{}, mounts)

	if err != nil {
		t.Error(err)
		t.FailNow()
	}

	if statusCode != 0 {
		t.Error("Second run should not have created a file")
	}

	if body1 != body2 {
		t.Errorf("%s\nShould have been equal to:\n%s\n", body1, body2)
	}

	c.RemoveVolume("persistentvolume")
}
