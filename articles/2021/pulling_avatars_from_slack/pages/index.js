const { PrismaClient } = require("@prisma/client");
const prisma = new PrismaClient();
const { MessageSender } = require("../components/messageSender");

function Home({ users }) {
  return (
    <div>
      <div className="d-flex flex-wrap justify-content-around">
        {users.map((u) => (
          <div key={u.id} className="card mt-3" style={{ width: "18rem" }}>
            <div className="card-body">
              {u.custom_image && (
                <img src={u.original_image} className="card-img-top" />
              )}
              <h5 className="card-title">{u.name}</h5>
              <p className="card-text">
                {u.real_name}
                <br />
                {u.title}
                <br />
                <a href={`mailto:${u.email}`}>{u.email}</a>
              </p>
              <MessageSender user={u} />
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

export async function getServerSideProps(context) {
  const users = await prisma.slackUser.findMany({
    where: {
      deleted: false,
      restricted: false,
    },
  });

  return {
    props: { users }, // will be passed to the page component as props
  };
}
export default Home;
