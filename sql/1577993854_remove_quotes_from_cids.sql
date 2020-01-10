-- Remove any quotes from the cid column in the User CID table.
update user_cid set cid = REPLACE(cid, '"', '')
