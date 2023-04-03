import Head from 'next/head';
import { prisma } from 'lib/prisma';
import { Post } from '.prisma/client';
import Link from 'next/link';
import { useForm } from 'react-hook-form';

export default function PostsPage({ posts }) {
  const {
    register,
    handleSubmit,
    formState: { errors, isSubmitted },
  } = useForm();

  const onSubmit = (data) => {
    try {
      fetch(`${window.location.origin}/api/create`, {
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json',
        },
        method: 'POST',
      }).then( () => {
        window.location.href = window.location.href
       } )
    } catch (error) {
      throw new Error(error);
    }
  };

    return (
      <>
        <ul>
            {posts.map((item: Post) => (
                <li key={item.id}>{item.title} - {item.content}</li>
            ))}
        </ul>
        <form onSubmit={handleSubmit(onSubmit)}>
          <label>Title</label>
          <input 
            id="title"
            type="text" 
            {...register('title', {required: true})}/>
            <br/>
          <label>Content</label>
          <input 
            type="text" 
            id="content"
            {...register('content', {required: true})}/>
          <input type="submit"/>
        </form>
        </>
    );
}

export const getServerSideProps = async () => {
  const posts = await prisma.post.findMany({
    select: {
      id: true,
      title: true,
      content: true
    },
  });

  console.log(posts);
  return {
    props: {
      posts,
    },
  };
};
