package com.example.multi_activity_app;

import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import android.content.Intent;
import android.os.Bundle;
import android.widget.Toast;

import com.example.multi_activity_app.R;


public class MainActivity extends AppCompatActivity implements RecyclerViewInterface {

    String[] buildingNames = null;
    int[] imageIds = null;
    RecyclerView list = null;
    ImageRecycleAdapter adapter = null;
    Team teamData = null;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // Initialize teamData here
        teamData = new Team(this);

        // wire list
        list = findViewById(R.id.recyclerView);
        list.setLayoutManager(new LinearLayoutManager(this));

        // Get the adapter data
        buildingNames = teamData.getMemberNames();
        String[] memberRoles = teamData.getMemberRoles();
        imageIds = teamData.getPhotoIds();

        // Pass the team member names, roles, and their respective image IDs to the adapter
        adapter = new ImageRecycleAdapter(getApplicationContext(), R.layout.item_layout, buildingNames, memberRoles, imageIds, this);
        list.setAdapter(adapter);
    }

        @Override
    public void onItemClick(int position) {
        Toast.makeText(this, "Selected " + buildingNames[position], Toast.LENGTH_SHORT).show();

        // Start SimpleDetailsActivity with extras
            Intent intent = new Intent(MainActivity.this, SimpleDetailsActivity.class);
            Bundle bundle = new Bundle();
            TeamMember member = teamData.getMember(position);

        // Update the keys to match what SimpleDetailsActivity is expecting
            bundle.putSerializable("name", member.getName());
            bundle.putSerializable("role", member.getRole());
            bundle.putSerializable("photo", member.getPhoto());
            bundle.putSerializable("bio", member.getBio());
            bundle.putSerializable("stats", member.getStats());
            bundle.putSerializable("link", member.getLink());
            intent.putExtras(bundle);

            startActivity(intent);
    }
}
