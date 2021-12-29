export const MessageSender = ({ user }) => {
  return (
    <form action="/api/sendMessage" method="get">
      <input
        type="text"
        name="message"
        placeholder={`message to ${user.name}`}
      />
      <input type="hidden" name="id" value={user.id} />
      <button type="submit" className="btn-primary">
        Send
      </button>
    </form>
  );
};
