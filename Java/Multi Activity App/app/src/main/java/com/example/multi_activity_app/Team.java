package com.example.multi_activity_app;

import android.content.Context;

import com.example.multi_activity_app.R;

import org.json.JSONArray;
import org.json.JSONObject;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class Team {
    private TeamMember[] members;
    private Context context;

    public Team(Context context) {
        this.context = context;
        loadTeamMembersFromJSON();
    }

    public int getCount() {
        return members.length;
    }

    public TeamMember getMember(int i) {
        return members[i];
    }

    public String[] getMemberNames() {
        String[] names = new String[getCount()];
        for (int i = 0; i < getCount(); i++) {
            names[i] = members[i].getName();
        }
        return names;
    }

    public int[] getPhotoIds() {
        int[] photoIds = new int[getCount()];
        for (int i = 0; i < getCount(); i++) {
            String photoName = members[i].getPhoto();
            photoName = photoName.substring(0, photoName.lastIndexOf("."));
            photoIds[i] = context.getResources().getIdentifier(photoName, "drawable", context.getPackageName());
        }
        return photoIds;
    }

    public String[] getMemberRoles() {
        String[] roles = new String[getCount()];
        for (int i = 0; i < getCount(); i++) {
            roles[i] = members[i].getRole();
        }
        return roles;
    }

    private void loadTeamMembersFromJSON() {
        String json = null;
        try {
            InputStream is = context.getResources().openRawResource(R.raw.team_members);
            int size = is.available();
            byte[] buffer = new byte[size];
            is.read(buffer);
            is.close();
            json = new String(buffer, StandardCharsets.UTF_8);

            // Parse the JSON data
            JSONArray jsonArray = new JSONArray(json);
            members = new TeamMember[jsonArray.length()];

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject jsonObject = jsonArray.getJSONObject(i);
                members[i] = new TeamMember(
                        jsonObject.getString("name"),
                        jsonObject.getString("role"),
                        jsonObject.getString("photo"),
                        jsonObject.getString("bio"),
                        jsonObject.getString("stats"),
                        jsonObject.getString("link")
                );
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
