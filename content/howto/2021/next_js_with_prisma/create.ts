import { prisma } from 'lib/prisma';

export default async function handler(req, res) {
  const { content, title } = req.body;

  try {
    const feedback = await prisma.post.create({
      data: {
        content,
        title,
      },
    });
    res.status(200).json(feedback);
  } catch (error) {
    res.status(400).json({
      message: `Something went wrong :/ ${error}`,
    });
  }
}
