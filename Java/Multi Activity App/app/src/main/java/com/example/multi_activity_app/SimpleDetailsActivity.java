package com.example.multi_activity_app;

import androidx.appcompat.app.AppCompatActivity;

import android.content.Intent;
import android.os.Bundle;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.example.multi_activity_app.R;

public class SimpleDetailsActivity extends AppCompatActivity {
    TextView nameTextView, roleTextView, bioTextView;
    ImageView photoImageView;
    Button fullDetailsButton, webInfoButton; // Declare the buttons here without initializing

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_simple_details);

        // text views and image views
        nameTextView = findViewById(R.id.nameTextView);
        roleTextView = findViewById(R.id.roleTextView);
        bioTextView = findViewById(R.id.bioTextView);
        photoImageView = findViewById(R.id.photoImageView);

        //  buttons
        fullDetailsButton = findViewById(R.id.fullDetailsButton);
        webInfoButton = findViewById(R.id.webInfoButton);

        // Handle the received intent
        Intent receivedIntent = getIntent();
        Bundle bundle = receivedIntent.getExtras();

        if (bundle != null) {
            // Set the text views and image view with the received data
            nameTextView.setText(bundle.getString("name"));
            roleTextView.setText(bundle.getString("role"));
            bioTextView.setText(bundle.getString("stats"));

            String photoName = bundle.getString("photo").split("\\.")[0];
            int photoId = getResources().getIdentifier(photoName, "drawable", getPackageName());
            photoImageView.setImageResource(photoId);

            // Set onClickListeners for buttons
            fullDetailsButton.setOnClickListener(view -> {
                Intent fullDetailsIntent = new Intent(SimpleDetailsActivity.this, FullDetailsActivity.class);
                // Pass all the details to FullDetailsActivity
                fullDetailsIntent.putExtra("name", bundle.getString("name"));
                fullDetailsIntent.putExtra("role", bundle.getString("role"));
                fullDetailsIntent.putExtra("bio", bundle.getString("bio"));
                fullDetailsIntent.putExtra("stats", bundle.getString("stats"));
                fullDetailsIntent.putExtra("photo", bundle.getString("photo"));
                startActivity(fullDetailsIntent);
            });


            webInfoButton.setOnClickListener(view -> {
                Intent webInfoIntent = new Intent(SimpleDetailsActivity.this, WebInfoActivity.class);
                webInfoIntent.putExtra("link", bundle.getString("link")); // Pass the link to WebInfoActivity
                startActivity(webInfoIntent);
            });
        }
    }
}
