-- CreateTable
CREATE TABLE "SlackUser" (
    "id" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "email" TEXT,
    "deleted" BOOLEAN NOT NULL,
    "admin" BOOLEAN,
    "restricted" BOOLEAN,
    "bot" BOOLEAN NOT NULL,
    "tz" TEXT,
    "title" TEXT,
    "skype" TEXT,
    "real_name" TEXT,
    "display_name" TEXT,
    "status_text" TEXT,
    "status_emoji" TEXT,
    "custom_image" BOOLEAN,
    "original_image" TEXT,

    CONSTRAINT "SlackUser_pkey" PRIMARY KEY ("id")
);
