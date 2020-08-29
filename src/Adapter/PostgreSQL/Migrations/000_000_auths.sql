CREATE EXTENSION citext;
CREATE EXTENSION pgcrypto;

-- DROP TABLE auths IF EXISTS;
-- DROP TABLE schema_migrations IF EXISTS;

CREATE TABLE auths (
  id bigserial PRIMARY KEY NOT NULL,
  pass citext NOT NULL,
  email citext NOT NULL UNIQUE,
  email_verification_code text NOT NULL,
  is_email_verified boolean NOT NULL
);
