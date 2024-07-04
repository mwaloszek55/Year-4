package com.example.multi_activity_app;

import androidx.appcompat.app.AppCompatActivity;
import android.content.Intent;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.TextView;

import com.example.multi_activity_app.R;

public class FullDetailsActivity extends AppCompatActivity {
    TextView nameTextView, roleTextView, bioHeader, bioTextView, statsHeader, statsTextView;
    ImageView imageViewDetails;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_full_details);

        imageViewDetails = findViewById(R.id.imageViewDetails);
        nameTextView = findViewById(R.id.nameTextView);
        roleTextView = findViewById(R.id.roleTextView);
        bioHeader = findViewById(R.id.bioHeader);
        bioTextView = findViewById(R.id.bioTextView);
        statsHeader = findViewById(R.id.statsHeader);
        statsTextView = findViewById(R.id.statsTextView);

        Intent intent = getIntent();
        if (intent != null) {
            nameTextView.setText(intent.getStringExtra("name"));
            roleTextView.setText(intent.getStringExtra("role"));
            bioTextView.setText(intent.getStringExtra("bio"));
            statsTextView.setText(intent.getStringExtra("stats"));

            String photoName = intent.getStringExtra("photo");
            if (photoName != null) {
                photoName = photoName.split("\\.")[0];
                int imageResource = getResources().getIdentifier(photoName, "drawable", getPackageName());
                imageViewDetails.setImageResource(imageResource != 0 ? imageResource : R.drawable.ic_launcher_background);
            }
        }

        // Set headers text
        bioHeader.setText("Bio");
        statsHeader.setText("Statistics");
    }
}
