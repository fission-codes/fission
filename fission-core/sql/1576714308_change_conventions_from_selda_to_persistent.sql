-- Move from Selda to Persistent

-- Update User table
-- 1. Rename to user
-- 2. Change columns to snake case
-- 3. Change text columns to varchar
ALTER SEQUENCE "users_userID_seq" RENAME TO user_id_seq;
ALTER TABLE users
  ALTER COLUMN "username" TYPE varchar,
  ALTER COLUMN "email" TYPE varchar,
  ALTER COLUMN "role" TYPE varchar,
  ALTER COLUMN "secretDigest" TYPE varchar;

ALTER TABLE users RENAME COLUMN "userID" TO id;
ALTER TABLE users RENAME COLUMN "herokuAddOnId" TO "heroku_add_on_id";
ALTER TABLE users RENAME COLUMN "secretDigest" TO "secret_digest";
ALTER TABLE users RENAME COLUMN "insertedAt" TO "inserted_at";
ALTER TABLE users RENAME COLUMN "modifiedAt" TO "modified_at";
ALTER TABLE users RENAME TO "user";

-- Update User CIDs
-- 1. Rename to user
-- 2. Change columns to snake case
-- 3. Change text columns to varchar
ALTER SEQUENCE "user_cids_userCID_seq" RENAME TO user_cid_id_seq;
ALTER TABLE user_cids ALTER COLUMN "cid" TYPE varchar;
ALTER TABLE user_cids RENAME COLUMN "userCID" TO id;
ALTER TABLE user_cids RENAME COLUMN "userFK" TO "user_fk";
ALTER TABLE user_cids RENAME COLUMN "insertedAt" TO "inserted_at";
ALTER TABLE user_cids RENAME COLUMN "modifiedAt" TO "modified_at";
ALTER TABLE user_cids RENAME TO "user_cid";

-- Update Herou Addon table
-- 1. Rename to user
-- 2. Change columns to snake case
-- 3. Change text columns to varchar
ALTER SEQUENCE "heroku_add_ons_addOnID_seq" RENAME TO heroku_add_on_id_seq;
ALTER TABLE heroku_add_ons ALTER COLUMN "region" TYPE varchar;
ALTER TABLE heroku_add_ons RENAME COLUMN "addOnID" TO id;
ALTER TABLE heroku_add_ons RENAME COLUMN "insertedAt" TO "inserted_at";
ALTER TABLE heroku_add_ons RENAME COLUMN "modifiedAt" TO "modified_at";
ALTER TABLE heroku_add_ons RENAME TO "heroku_add_on";

-- Delete duplicate user_id cid pairs
WITH groupedCids AS (
    SELECT
        id,
        user_fk,
        cid,
        ROW_NUMBER() OVER (
            PARTITION BY
                user_fk,
                cid
            ORDER BY
                user_fk,
                cid
        ) row_num
     FROM
        user_cid
) DELETE FROM user_cid where id in (
  SELECT id from groupedCids where row_num > 1
);
